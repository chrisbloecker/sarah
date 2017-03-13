{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave.Messages
  where
--------------------------------------------------------------------------------
import Data.Binary                 (Binary)
import Data.Typeable               (Typeable)
import Control.Distributed.Process (Process, ProcessId, send)
import GHC.Generics                (Generic)
--------------------------------------------------------------------------------

data GetStatus = GetStatus deriving (Binary, Generic, Typeable)
data Terminate = Terminate deriving (Binary, Generic, Typeable)

--------------------------------------------------------------------------------

getStatus :: ProcessId -> Process ()
getStatus pid = send pid GetStatus
