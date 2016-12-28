{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master.Messages
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process (Process, ProcessId, send)
import Import.MkBinary
import Sarah.Middleware.Model
--------------------------------------------------------------------------------

data NodeUp    = NodeUp    ProcessId NodeInfo deriving (Binary, Generic, Typeable)
data GetStatus = GetStatus ProcessId          deriving (Binary, Generic, Typeable)

--------------------------------------------------------------------------------

nodeUp :: Master -> ProcessId -> NodeInfo -> Process ()
nodeUp (Master master) pid nodeInfo = send master (NodeUp pid nodeInfo)

getStatus :: Master -> ProcessId -> Process ()
getStatus (Master master) pid = send master (GetStatus pid)
