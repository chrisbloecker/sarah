{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module  Master.Messages
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process                (Process, send)
import Control.Distributed.Process.Internal.Types (NodeId)
import Import.MkBinary
import Types                                      (Master (Master))
--------------------------------------------------------------------------------

data NodeUp = NodeUp NodeId deriving (Generic, Typeable)
instance Binary NodeUp

--------------------------------------------------------------------------------

nodeUp :: Master -> NodeId -> Process ()
nodeUp (Master master) node = send master (NodeUp node)