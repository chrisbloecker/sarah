{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master
  ( MasterSettings (..)
  , runMaster
  , masterName
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Lens
import Data.Map
import Data.Text                        (unpack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Import.DeriveJSON
import Network.HTTP.Client              (Manager)
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model           hiding (manager)
import Sarah.Middleware.Util
import Servant.Common.BaseUrl           (BaseUrl)
--------------------------------------------------------------------------------
import qualified Sarah.Persist.Model  as Persist
import qualified Sarah.Persist.Client as Persist
--------------------------------------------------------------------------------

data MasterSettings = MasterSettings { masterNode :: WebAddress
                                     , backend    :: WebAddress
                                     , webPort    :: Port
                                     }
deriveJSON jsonOptions ''MasterSettings

data State = State { _nodes   :: Map ProcessId NodeInfo
                   , _manager :: Manager
                   , _persist :: BaseUrl
                   }
makeLenses ''State

--------------------------------------------------------------------------------

runMaster :: Manager -> BaseUrl -> Process ()
runMaster manager persist = do
  self <- getSelfPid
  register masterName self
  say "Master up"
  loop $ State empty manager persist


loop :: State -> Process ()
loop state =
  receiveWait [ match $ \(GetStatus pid) -> do
                  say "Received GetStatus message"
                  send pid (Status $ state^.nodes^..folded)
                  loop state

              , match $ \(Log nodeName message logLevel) -> do
                  say "Received Log message"
                  -- ToDo: pass the result back to the master instead of just printing something
                  spawnLocal $ do
                    now <- liftIO getCurrentTime
                    let logEntry = Persist.Log (utctDay now) (timeToTimeOfDay . utctDayTime $ now) nodeName message logLevel
                    mRes <- runEIO $ Persist.putLog logEntry (state^.manager) (state^.persist)
                    case mRes of
                      Left err -> say (show err)
                      Right _  -> return ()
                  loop state

              , match $ \(NodeUp pid nodeInfo) -> do
                  say . unwords $ [ nodeInfo^.nodeName & unpack
                                  , "connected"
                                  , show pid
                                  ]
                  mon <- monitor pid
                  loop $ state & nodes.at pid .~ Just nodeInfo

              , match $ \(SensorReading room sensor value) -> do
                  say "Received SensorReading message"
                  -- ToDo: same here, pass the result back to the master
                  spawnLocal $ do
                    now <- liftIO getCurrentTime
                    let sensorReading = Persist.SensorReading (utctDay now) (timeToTimeOfDay . utctDayTime $ now) room sensor value
                    mRes <- runEIO $ Persist.putSensorReading sensorReading (state^.manager) (state^.persist)
                    case mRes of
                      Left err -> say (show err)
                      Right _  -> return ()
                  loop state

              , match $ \(ProcessMonitorNotification monRef pid reason) -> do
                  say . unwords $ [ state^.nodes.at pid._Just.nodeName & unpack
                                  , show reason
                                  , show pid
                                  ]
                  loop $ state & nodes.at pid .~ Nothing
              ]
