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
import Control.Monad                            (forM, void)
import Data.Aeson.Types                         (Value (..))
import Data.List                                ((\\), elem, notElem)
import Data.Map.Strict                          (Map)
import Data.Monoid                              ((<>))
import Data.Text                                (unpack)
import Import.DeriveJSON
import Raspberry.Hardware
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed             (NodeInfo (..))
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model            hiding (master, nodeName)
import Sarah.Middleware.Model.Interface
import Sarah.Middleware.Slave.Messages
import Sarah.Middleware.Util
import Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map.Strict   as M  (fromList, empty, insert, lookup, foldrWithKey)
import qualified Data.HashMap.Lazy as HM (fromList)
--------------------------------------------------------------------------------

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

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  mmaster <- findMaster (host masterAddress) (show . port $ masterAddress) (seconds 1)
  case mmaster of
    Nothing -> do
      say "[slave] No master found, terminating"
      -- make sure there's enough time to print the message
      liftIO $ threadDelay 100000
    Just master -> do
      say $ "[slave] found master at " ++ show master
      self              <- getSelfPid
      let slave         = mkSlave self
          queryTimeout  = timeout
      portManager       <- startPortManager
      say "[slave] starting device controllers"
      deviceControllers <- fmap M.fromList <$> forM devices $ \(DeviceDescription name (Device model)) -> do say $ "[slave] starting " ++ show name
                                                                                                             pid <- startDeviceController model slave portManager
                                                                                                             return (name, pid)

      nodeUp master self (NodeInfo nodeName [ (name, toDeviceRep device) | (DeviceDescription name device) <- devices ])
      link (unMaster master)

      let reverseLookup = M.foldrWithKey (\deviceName (DeviceController pid) -> M.insert pid deviceName) M.empty deviceControllers
      loop State{..}

loop :: State -> Process ()
loop state@State{..} =
  receiveWait [ match $ \(FromPid src (query :: Query)) -> do
                  -- ToDo: should we check if the query was intended for this node?
                  void $ spawnLocal $ do
                    let deviceName' = deviceName (queryTarget query)
                    say $ "[slave] Received query for " ++ show deviceName'
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

              , matchAny $ \message -> do
                  say $ "[slave] Received unexpected message: " ++ show message
                  loop state
              ]
