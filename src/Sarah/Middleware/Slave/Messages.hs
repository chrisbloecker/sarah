{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave.Messages
  where
--------------------------------------------------------------------------------
import Data.Aeson                               (ToJSON, FromJSON, encode)
import Data.Binary                              (Binary)
import Data.ByteString.Lazy                     (ByteString)
import Data.Typeable                            (Typeable)
import Control.Distributed.Process              (Process, ProcessId, send)
import Control.Distributed.Process.Serializable (Serializable)
import GHC.Generics                             (Generic)
import Sarah.Middleware.Model
--------------------------------------------------------------------------------

data GetStatus    = GetStatus                       deriving (Binary, Generic, Typeable)
data Terminate    = Terminate                       deriving (Binary, Generic, Typeable)
data StateChanged = StateChanged EncodedDeviceState deriving (Binary, Generic, Typeable)

--------------------------------------------------------------------------------

getStatus :: ProcessId -> Process ()
getStatus pid = send pid GetStatus

sendSlave :: Serializable a => Slave -> a -> Process ()
sendSlave slave = send (unSlave slave)

sendStateChanged :: (IsDevice model) => Slave -> DeviceState model -> Process ()
sendStateChanged slave state = sendWithPid (unSlave slave) (encodeDeviceState state)
