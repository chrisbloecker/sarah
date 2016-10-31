{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Master
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Messages
--------------------------------------------------------------------------------

master :: Process ()
master = do
  self <- getSelfPid
  receiveWait [ match $ \EchoMsg {..} -> do
                  say $ "[INFO] Message from " ++ show echoMsgSender
                  send echoMsgSender (EchoMsg self)
                  master
              ]
