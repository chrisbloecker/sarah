{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , DeviceDescription (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent                       (threadDelay)
import Control.Distributed.Process
import Control.Monad                            (forM, void, forever, unless)
import Data.Aeson.Types                         (Value (..))
import Data.List                                ((\\), elem, notElem)
import Data.Map.Strict                          (Map)
import Data.Monoid                              ((<>))
import Data.Text                                (unpack)
import Data.Time.Calendar                       (Day (..), fromGregorian, toGregorian, diffDays)
import Data.Time.Clock                          (UTCTime (..), getCurrentTime, diffUTCTime)
import Data.Time.LocalTime                      (TimeOfDay (..), timeOfDayToTime)
import Import.DeriveJSON
import Raspberry.Hardware
import Sarah.Middleware.Database
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed             (NodeInfo (..))
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model            hiding (master, nodeName)
import Sarah.Middleware.Model.Interface
import Sarah.Middleware.Slave.Messages
import Sarah.Middleware.Util
--------------------------------------------------------------------------------
import qualified Data.Map.Strict   as M  (fromList, empty, insert, lookup, foldrWithKey)
import qualified Data.HashMap.Lazy as HM (fromList)
--------------------------------------------------------------------------------

-- | A device is described by its name and the device itself.
data DeviceDescription = DeviceDescription DeviceName Device deriving (Show)

instance ToJSON DeviceDescription where
  toJSON (DeviceDescription name device) = Object $ toObject device <> HM.fromList [ "name" .= toJSON name ]
    where
      -- ToDo: this should work but is really not nice
      toObject :: ToJSON a => a -> Object
      toObject a = case toJSON a of
        Object o -> o
        _        -> error "Not an object" -- ToDo: something more informative

instance FromJSON DeviceDescription where
  parseJSON = withObject "DeviceDescription" $ \o ->
    DeviceDescription <$> o .: "name"
                      <*> parseJSON (Object o)

data SlaveSettings = SlaveSettings { nodeName      :: NodeName
                                   , nodeAddress   :: WebAddress
                                   , masterAddress :: WebAddress
                                   , devices       :: [DeviceDescription]
                                   , timeout       :: Int
                                   }
  deriving (Show)
deriveJSON jsonOptions ''SlaveSettings

data State = State { deviceControllers :: Map DeviceName DeviceController
                   , reverseLookup     :: Map ProcessId  DeviceName
                   , master            :: Master
                   , nodeName          :: NodeName
                   , queryTimeout      :: Int
                   }

--------------------------------------------------------------------------------

startPortManager :: Process PortManager
startPortManager = do
  say "[startPortManager]"
  PortManager <$> spawnLocal portManager
    where
      portManager :: Process ()
      portManager = receiveWait [ matchAny $ \m -> do
                                    say $ "[portManager] Received unexpected message: " ++ show m
                                    portManager
                                ]

waitUntil :: UTCTime -> IO ()
waitUntil until = do
  now <- getCurrentTime
  let µsUntil = 10^6 * (floor . toRational $ diffUTCTime until now)
  unless (µsUntil <= 0) $
    if µsUntil > fromIntegral (maxBound :: Int)
      then threadDelay (maxBound :: Int) >> waitUntil until
      else threadDelay µsUntil


runSchedule :: Schedule -> ProcessId -> Process ProcessId
runSchedule Schedule{..} devicePid = spawnLocal $ case scheduleTimer of
  Once       day timeOfDay -> runOnce       day timeOfDay scheduleAction devicePid
  Every      timePoint     -> runEvery      timePoint     scheduleAction devicePid
  Repeatedly timeInterval  -> runRepeatedly timeInterval  scheduleAction devicePid

  where
    runOnce :: Day -> TimeOfDay -> Query -> ProcessId -> Process ()
    runOnce day timeOfDay query devicePid = do
      now <- liftIO getCurrentTime
      let until = UTCTime day (timeOfDayToTime timeOfDay)
      unless (until < now) $ do
        liftIO $ waitUntil until
        say $ "[runOnce] running " ++ show query
        sendWithPid devicePid query

    runEvery :: TimePoint -> Query -> ProcessId -> Process ()
    runEvery timePoint query devicePid = do
      now      <- liftIO getCurrentTime
      nextTime <- liftIO $ nextOccurence timePoint

      liftIO $ waitUntil nextTime
      say $ "[runEvery] running " ++ show query
      sendWithPid devicePid query
      runEvery timePoint query devicePid

    runRepeatedly :: TimeInterval -> Query -> ProcessId -> Process ()
    runRepeatedly timeInterval@(TimeInterval t) query devicePid = do
      liftIO $ threadDelay (t * 10^6)
      say $ "[runRepeatedly] running " ++ show query
      sendWithPid devicePid query
      runRepeatedly timeInterval query devicePid


runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = forever $ do -- "reconnect" when the connection is lost
  mmaster <- findMaster (host masterAddress) (show . port $ masterAddress) (seconds 1)
  case mmaster of
    Nothing -> do
      say "[slave] No master found, retrying"
      -- make sure there's enough time to print the message
      liftIO $ threadDelay 100000
    Just master -> do
      say $ "[slave] found master at " ++ show master
      monitor (unMaster master)
      self              <- getSelfPid
      let slave         = mkSlave self
          queryTimeout  = timeout
      portManager       <- startPortManager
      say "[slave] starting device controllers"
      forDevices <- forM devices $ \(DeviceDescription name (Device model)) -> do
                      say $ "[slave] starting " ++ show name
                      pid <- startDeviceController model slave portManager
                      say $ "[slave] requesting schedule for " ++ show name
                      sendWithPid (unMaster master) (GetScheduleRequest $ DeviceAddress nodeName name)
                      GetScheduleReply schedule <- expect
                      say "[slave] setting up schedule..."
                      triggers <- forM schedule $ \(scheduleId, scheduleItem) ->
                                    (,) <$> pure scheduleId <*> runSchedule scheduleItem (unDeviceController pid)
                      say $ "[slave] device " ++ show name ++ " initialised."
                      return (name, pid, triggers)

      let reverseLookup     = M.foldrWithKey (\deviceName (DeviceController pid) -> M.insert pid deviceName) M.empty deviceControllers
          deviceControllers = M.fromList [(name, pid) | (name, pid, _) <- forDevices]

      nodeUp master self (NodeInfo nodeName [ (name, toDeviceRep device) | (DeviceDescription name device) <- devices ])

      loop State{..}

loop :: State -> Process ()
loop state@State{..} =
  receiveWait [ match $ \(FromPid src (query :: Query)) -> do
                  -- ToDo: should we check if the query was intended for this node?
                  let deviceName' = deviceName (queryTarget query)
                  say $ "[slave] Received query for " ++ show deviceName'
                  spawnLocal $
                    case M.lookup deviceName' deviceControllers of
                      Nothing                      -> say $ "[slave] Unknown device: " ++ unpack deviceName'
                      Just (DeviceController dest) -> void $ spawnLocal $ do
                        sendWithPid dest query
                        mr <- receiveTimeout queryTimeout [ match    $ \(result :: QueryResult) -> send src result
                                                          , matchAny $ \m                       -> say $ "[slave] Unexpected message: " ++ show m
                                                          ]
                        case mr of
                          Nothing -> say $ "[slave] Timeout on query to " ++ unpack deviceName' ++ ". Query was " ++ show (queryCommand query)
                          Just _ -> return ()
                  loop state

              , match $ \(FromPid src (StateChanged encodedState)) -> do
                  say $ "[slave] A device changed its state at node: " ++ show src
                  case M.lookup src reverseLookup of
                    Nothing         -> say $ "[slave] Unknown pid: " ++ show src
                    Just deviceName -> do
                      say $ "[slave] Device that changed its state: " ++ unpack nodeName ++ ":" ++ unpack deviceName
                      sendWithPid (unMaster master) $ DeviceStateChanged (DeviceAddress nodeName deviceName) encodedState
                  loop state

              , match $ \Terminate ->
                  say "[slave] Terminating slave"

              , match $ \(ProcessMonitorNotification monRef pid reason) ->
                  say "[slave] Master went down, terminating"

              , matchAny $ \message -> do
                  say $ "[slave] Received unexpected message: " ++ show message
                  loop state
              ]
