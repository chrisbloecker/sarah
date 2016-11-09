{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Slave
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Network.Socket              (HostName, ServiceName)
import Util
--------------------------------------------------------------------------------
import Master.Messages             (nodeUp)
import Slave.Messages              (Terminate (Terminate))
--------------------------------------------------------------------------------

slave :: HostName -> ServiceName -> Process ()
slave masterHost masterPort = do
  thisNode <- getSelfNode
  mmaster  <- findMaster masterHost masterPort (seconds 1)
  case mmaster of
    Nothing -> say "No master found... Terminating..."
    Just master -> do
      nodeUp master thisNode
      linkMaster master
      Terminate <- expect
      return ()
