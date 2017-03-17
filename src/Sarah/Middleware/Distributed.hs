{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Distributed
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process (Process, ProcessId, getSelfPid, send)
import Data.Aeson                  (ToJSON, FromJSON)
import Data.Binary                 (Binary)
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)
import Sarah.Middleware.Device     (DeviceRep)
import Sarah.Middleware.Types      (DeviceName, EncodedJSON, FromPid (..), NodeName)
--------------------------------------------------------------------------------

-- like send, but wraps the message with the pid of the sending process
sendWithPid :: (Binary message, Typeable message) => ProcessId -> message -> Process ()
sendWithPid to message = getSelfPid >>= \self -> send to (FromPid self message)


data NodeInfo = NodeInfo { nodeName    :: NodeName
                         , nodeDevices :: [(DeviceName, DeviceRep)]
                         }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

data Status = Status { connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

--------------------------------------------------------------------------------
