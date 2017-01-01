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
import Import.DeriveJSON
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model
import Sarah.Middleware.Util
--------------------------------------------------------------------------------

data MasterSettings = MasterSettings { masterNode :: WebAddress
                                     , backend    :: WebAddress
                                     , webPort    :: Port
                                     }
  deriving (Show)
deriveJSON jsonOptions ''MasterSettings

data State = State { _nodes :: Map ProcessId NodeInfo }
makeLenses ''State

initialState :: State
initialState = State { _nodes = empty }

--------------------------------------------------------------------------------

runMaster :: Process ()
runMaster = do
  self <- getSelfPid
  register masterName self
  say "Master up"
  loop initialState

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \(NodeUp pid nodeInfo) -> do
                  say . unwords $ [ nodeInfo^.nodeName & unpack
                                  , "connected"
                                  , show pid
                                  ]
                  mon <- monitor pid
                  loop $ state & nodes.at pid .~ Just nodeInfo

              , match $ \(GetStatus pid) -> do
                  send pid (Status $ state^.nodes^..folded)
                  loop state

              , match $ \(ProcessMonitorNotification monRef pid reason) -> do
                  say . unwords $ [ state^.nodes.at pid._Just.nodeName & unpack
                                  , show reason
                                  , show pid
                                  ]
                  loop $ state & nodes.at pid .~ Nothing
              ]
