{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master
  ( MasterSettings (..)
  , runMaster
  , masterName
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent.STM           (TVar, atomically, readTVar)
import Control.Distributed.Process
import Control.Monad                    (void, forM_)
import Data.Map.Strict                  (Map, empty, elems, insert, delete)
import Data.Text                        (unpack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Generics                     (Generic)
import Import.DeriveJSON
import Network.HTTP.Client              (Manager)
import Network.WebSockets               (Connection, sendTextData)
import Raspberry.Hardware
import Sarah.Middleware.Distributed     (NodeInfo (..), Status (..))
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model           hiding (manager)
import Sarah.Middleware.Types
import Sarah.Middleware.Util
import Servant.Client
--------------------------------------------------------------------------------
import qualified Data.Map.Strict      as M
import qualified Sarah.Persist.Model  as Persist
import qualified Sarah.Persist.Client as Persist
--------------------------------------------------------------------------------

data MasterSettings = MasterSettings { masterNode :: WebAddress
                                     , backend    :: WebAddress
                                     , webPort    :: Port
                                     }
  deriving (Generic, FromJSON)

data State = State { nodes          :: Map ProcessId NodeInfo
                   , nodeNames      :: Map NodeName  ProcessId
                   , backendClient  :: ClientEnv
                   , subscribers    :: TVar [(Integer, Connection)]
--                   , manager        :: Manager
--                   , persistBackend :: BaseUrl
                   }

--------------------------------------------------------------------------------

runMaster :: ClientEnv -> TVar [(Integer, Connection)] -> Process ()
runMaster backendClient subscribers = do
  self <- getSelfPid
  register masterName self
  say "Master up"
  loop State { nodes         = empty
             , nodeNames     = empty
             , backendClient = backendClient
             , subscribers   = subscribers
             }


loop :: State -> Process ()
loop state@State{..} =
  receiveWait [ match $ \(FromPid pid GetStatusRequest) -> do
                  say $ "[master] Status requested by " ++ show pid
                  send pid (GetStatusReply $ Status (elems nodes))
                  loop state

              , match $ \(FromPid src query@Query{..}) -> do
                  say "[master] Received Query"
                  let nodeName = deviceNode queryTarget
                  case M.lookup nodeName nodeNames of
                    Nothing   -> say $ "[master] Unknown node name " ++ unpack nodeName
                    -- ToDo: is it safe to spawn a process and continue there?
                    --       could some data change or a process die?
                    Just dest -> void $ spawnLocal $ do
                      sendWithPid dest query
                      receiveWait [ match    $ \result@QueryResult{..} -> send src result
                                  , matchAny $ \m                      -> say $ "[master] Unexpected message: " ++ show m
                                  ]
                  loop state

                -- whenever the state of a device changes, we're informing all subscribers
                -- about the change and send them the encoded device address and state
              , match $ \(FromPid src (DeviceStateChanged deviceAddress@DeviceAddress{..} encodedState)) -> do
                  say $ "[master] A device changed its state: " ++ unpack deviceNode ++ ":" ++ unpack deviceName
                  spawnLocal $ do
                    say "[master] Broadcasting new device state"
                    liftIO $ do
                      connections <- atomically $ readTVar subscribers
                      forM_ connections $ \(_, connection) ->
                        sendTextData connection $ encodeAsText (deviceAddress, encodedState)
                  loop state

              , match $ \(Log nodeName message logLevel) -> do
                  say "Received Log message"
                  -- ToDo: pass the result back to the master instead of just printing something
                  spawnLocal $ do
                    now <- liftIO getCurrentTime
                    let logEntry = Persist.Log (utctDay now) (timeToTimeOfDay . utctDayTime $ now) nodeName message logLevel
                    mRes <- liftIO $ runClientM (Persist.putLog logEntry) backendClient
                    case mRes of
                      Left err -> say (show err)
                      Right _  -> return ()
                  loop state

              , match $ \(NodeUp pid nodeInfo@NodeInfo{..}) -> do
                  mon <- monitor pid
                  say $ "[master] " ++ unpack nodeName ++ " connected at " ++ show pid
                  loop $ state { nodes     = insert pid      nodeInfo nodes
                               , nodeNames = insert nodeName pid      nodeNames
                               }

              , match $ \(SensorReading room sensor value) -> do
                  say "Received SensorReading message"
                  -- ToDo: same here, pass the result back to the master
                  spawnLocal $ do
                    now <- liftIO getCurrentTime
                    let sensorReading = Persist.SensorReading (utctDay now) (timeToTimeOfDay . utctDayTime $ now) room sensor value
                    mRes <- liftIO $ runClientM (Persist.putSensorReading sensorReading) backendClient
                    case mRes of
                      Left err -> say (show err)
                      Right _  -> return ()
                  loop state

              , match $ \(ProcessMonitorNotification monRef pid reason) ->
                  case M.lookup pid nodes of
                    Nothing -> do
                      say $ "Received unexpected monitor notification from " ++ show reason ++ show pid
                      loop state
                    Just NodeInfo{..} -> do
                      say $ "Received monitor notification from " ++ unpack nodeName ++ ": " ++ show reason ++ " " ++ show pid
                      loop $ state { nodes     = pid      `delete` nodes
                                   , nodeNames = nodeName `delete` nodeNames
                                   }

              , matchAny $ \message -> do
                  say $ "[master] Received unexpected message: " ++ show message
                  loop state
              ]
