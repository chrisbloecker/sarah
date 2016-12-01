{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( runSlave
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Network.Socket                   (HostName, ServiceName)
import Sarah.Middleware.Master.Messages (nodeUp)
import Sarah.Middleware.Settings
import Sarah.Middleware.Slave.Messages  (Terminate (Terminate))
import Sarah.Middleware.Util
--------------------------------------------------------------------------------

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  thisNode <- getSelfNode
  mmaster  <- findMaster masterHost (show masterPort) (seconds 1)
  case mmaster of
    Nothing -> say "No master found... Terminating..."
    Just master -> do
      nodeUp master thisNode
      linkMaster master
      Terminate <- expect
      return ()
