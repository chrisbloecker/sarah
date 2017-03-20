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
import Sarah.Middleware.Model      (NodeName, DeviceName)
--------------------------------------------------------------------------------

data NodeInfo = NodeInfo { nodeName    :: NodeName
                         , nodeDevices :: [(DeviceName, DeviceRep)]
                         }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

data Status = Status { connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

--------------------------------------------------------------------------------
