{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Slave
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process (Process, getSelfPid)
import Network.Socket              (HostName, ServiceName)
import Messages
import Util
--------------------------------------------------------------------------------

slave :: HostName -> ServiceName -> Process ()
slave masterHost masterPort = do
  self <- getSelfPid
  mmaster <- findMaster masterHost masterPort (seconds 1)
  case mmaster of
    Nothing -> putStrLn "[ERROR] No master found... Terminating..."
    Just master -> do
      nodeUp master self
      linkMaster master
      Terminate <- expect
      return ()
