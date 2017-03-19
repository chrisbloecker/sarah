{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave.Messages
  where
--------------------------------------------------------------------------------
import Data.Aeson                               (ToJSON, FromJSON)
import Data.Binary                              (Binary)
import Data.Typeable                            (Typeable)
import Control.Distributed.Process              (Process, ProcessId, send)
import Control.Distributed.Process.Serializable (Serializable)
import GHC.Generics                             (Generic)
import Sarah.Middleware.Model
import Sarah.Middleware.Types
--------------------------------------------------------------------------------

data GetStatus    = GetStatus                deriving (Binary, Generic, Typeable)
data Terminate    = Terminate                deriving (Binary, Generic, Typeable)
data StateChanged = StateChanged EncodedJSON deriving (Binary, Generic, Typeable)

--------------------------------------------------------------------------------

getStatus :: ProcessId -> Process ()
getStatus pid = send pid GetStatus

sendSlave :: Serializable a => Slave -> a -> Process ()
sendSlave (Slave slave) = send slave

sendStateChanged :: (ToJSON state, FromJSON state) => Slave -> state -> Process ()
sendStateChanged (Slave slave) state = send slave (StateChanged $ encodeAndWrap state)
