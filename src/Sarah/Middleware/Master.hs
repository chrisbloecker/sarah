{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master
  ( MasterSettings (..)
  , runMaster
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent.STM           (TVar, atomically, readTVar)
import Control.Distributed.Process
import Control.Monad                    (void, forM_)
import Control.Concurrent               (forkIO)
import Data.Map.Strict                  (Map)
import Data.Text                        (unpack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Database.Persist.Sql             (ConnectionPool, runSqlPool)
import GHC.Generics                     (Generic)
import Import.DeriveJSON
import Network.HTTP.Client              (Manager)
import Network.WebSockets               (Connection, sendTextData)
import Raspberry.Hardware
import Sarah.Middleware.Database
import Sarah.Middleware.Distributed     (NodeInfo (..), Status (..))
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model           hiding (manager)
import Sarah.Middleware.Util
--------------------------------------------------------------------------------
import qualified Data.Map.Strict  as M
import qualified Database.Persist as DB
--------------------------------------------------------------------------------

data MasterSettings = MasterSettings { masterNode :: WebAddress
                                     , webPort    :: Port
                                     , timeout    :: Int
                                     , dbHost     :: String
                                     , dbPort     :: Int
                                     , dbUser     :: String
                                     , dbPassword :: String
                                     , dbDatabase :: String
                                     }
  deriving (Generic, FromJSON)

data State = State { nodes        :: Map ProcessId NodeInfo
                   , nodeNames    :: Map NodeName  ProcessId
                   , subscribers  :: TVar [(Integer, Connection)]
                   , queryTimeout :: Int
                   , pool         :: ConnectionPool
                   }

--------------------------------------------------------------------------------

runMaster :: TVar [(Integer, Connection)] -> Int -> ConnectionPool -> Process ()
runMaster subscribers queryTimeout pool = do
  self <- getSelfPid
  register masterName self
  say "Master up"
  loop State { nodes        = M.empty
             , nodeNames    = M.empty
             , subscribers  = subscribers
             , queryTimeout = queryTimeout
             , pool         = pool
             }


loop :: State -> Process ()
loop state@State{..} =
  receiveWait [ match $ \(FromPid pid (request :: MRequest GetStatus)) -> do
                  say $ "[master] Status requested by " ++ show pid
                  send pid (GetStatusReply $ Status (M.elems nodes))
                  loop state

              , match $ \(FromPid pid (request :: MRequest GetSchedule)) -> do
                  say $ "[master] Schedule requested by " ++ show pid
                  spawnLocal $ do
                    schedule <- liftIO $ runSqlPool (DB.selectList [] []) pool
                    send pid (GetScheduleReply $ fmap DB.entityVal schedule)
                  loop state

              , match $ \(FromPid src (query :: Query)) -> do
                  let nodeName = deviceNode (queryTarget query)
                  say $ "[master] Received Query for " ++ show nodeName
                  spawnLocal $
                    case M.lookup nodeName nodeNames of
                      Nothing   -> say $ "[master] Unknown node name " ++ unpack nodeName
                      -- ToDo: is it safe to spawn a process and continue there?
                      --       could some data change or a process die?
                      Just dest -> void $ spawnLocal $ do
                        sendWithPid dest query
                        mr <- receiveTimeout queryTimeout [ match    $ \(result :: QueryResult) -> send src result
                                                          , matchAny $ \m                       -> say $ "[master] Unexpected message: " ++ show m
                                                          ]
                        case mr of
                          Nothing -> say $ "[master] Timeout on query to " ++ unpack nodeName ++ ". Query was " ++ show (queryCommand query)
                          Just _  -> return ()
                  loop state

                -- whenever the state of a device changes, we inform all subscribers
                -- about the change and send them the encoded device address and state
              , match $ \(FromPid src (DeviceStateChanged deviceAddress@DeviceAddress{..} encodedState)) -> do
                  say $ "[master] A device changed its state: " ++ unpack deviceNode ++ ":" ++ unpack deviceName
                  spawnLocal $ do
                    connections <- liftIO . atomically $ readTVar subscribers
                    say $ "[master] Broadcasting new device state to " ++ show (length connections) ++ " listeners"
                    liftIO $ forM_ connections $ \(_, connection) ->
                      sendTextData connection $ StateChangeEvent deviceAddress encodedState
                  loop state

              , match $ \(PutLog nodeName message logLevel) -> do
                  say "Received Log message"
                  liftIO . forkIO $ do
                    now <- getCurrentTime
                    let today    = utctDay now
                        thisTime = timeToTimeOfDay . utctDayTime $ now
                        logEntry = Log today thisTime nodeName message logLevel
                    void $ runSqlPool (DB.insert logEntry) pool
                  loop state

              , match $ \(NodeUp pid nodeInfo@NodeInfo{..}) -> do
                  mon <- monitor pid
                  say $ "[master] " ++ unpack nodeName ++ " connected at " ++ show pid
                  loop $ state { nodes     = M.insert pid      nodeInfo nodes
                               , nodeNames = M.insert nodeName pid      nodeNames
                               }

              , match $ \(LogSensorReading room sensor value) -> do
                  say "Received SensorReading message"
                  liftIO . forkIO $ do
                    now <- liftIO getCurrentTime
                    let today         = utctDay now
                        thisTime      = timeToTimeOfDay . utctDayTime $ now
                        sensorReading = SensorReading today thisTime room sensor value
                    void $ runSqlPool (DB.insert sensorReading) pool
                  loop state

              , match $ \(ProcessMonitorNotification monRef pid reason) ->
                  case M.lookup pid nodes of
                    Nothing -> do
                      say $ "Received unexpected monitor notification from " ++ show reason ++ show pid
                      loop state
                    Just NodeInfo{..} -> do
                      say $ "Received monitor notification from " ++ unpack nodeName ++ ": " ++ show reason ++ " " ++ show pid
                      loop $ state { nodes     = M.delete pid      nodes
                                   , nodeNames = M.delete nodeName nodeNames
                                   }

              , matchAny $ \message -> do
                  say $ "[master] Received unexpected message: " ++ show message
                  loop state
              ]
