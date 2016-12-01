{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master
  ( runMaster
  , masterName
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Util
--------------------------------------------------------------------------------

data State = State { nodes :: [NodeId] }

--------------------------------------------------------------------------------

runMaster :: Process ()
runMaster = do
  self <- getSelfPid
  register masterName self
  say "Master up"
  loop $ State []

loop :: State -> Process ()
loop state@State{..} = receiveWait [ match $ \(NodeUp nid) -> do
                                       say $ "[INFO] Node up " ++ show nid
                                       loop state { nodes = nid : nodes }
                                   ]
