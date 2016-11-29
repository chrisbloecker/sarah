{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master
  ( master
  , masterName
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Util
--------------------------------------------------------------------------------

data State = State { nodes :: [NodeId] }

--------------------------------------------------------------------------------

master :: Process ()
master = do
  self <- getSelfPid
  register masterName self
  say "Master up"
  loop $ State []

loop :: State -> Process ()
loop state@State{..} = receiveWait [ match $ \(NodeUp nid) -> do
                                       say $ "[INFO] Node up " ++ show nid
                                       loop state { nodes = nid : nodes }
                                   ]
