{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Master
  ( master
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Messages
--------------------------------------------------------------------------------

data State = State

initState :: State
initState = State

--------------------------------------------------------------------------------

master :: Process ()
master = loop initState

loop :: State -> Process ()
loop state = receiveWait [ match $ \EchoMsg {..} -> do
                             say $ "[INFO] Message from " ++ show echoMsgSender
                             send echoMsgSender . EchoMsg =<< getSelfPid
                             loop state
                         ]
