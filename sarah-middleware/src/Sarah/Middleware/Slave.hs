{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Network.Socket                   (HostName, ServiceName)
import Sarah.Middleware.Master.Messages (nodeUp)
import Sarah.Middleware.Slave.Messages  (Terminate (Terminate))
import Sarah.Middleware.Util
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
