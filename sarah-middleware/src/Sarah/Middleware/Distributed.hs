{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Distributed
  where
--------------------------------------------------------------------------------
import Control.Lens
import Data.Binary             (Binary)
import Data.Typeable           (Typeable)
import GHC.Generics            (Generic)
import Sarah.Middleware.Device (DeviceRep)
import Sarah.Middleware.Types  (DeviceName, NodeName)
--------------------------------------------------------------------------------

data NodeInfo = NodeInfo { _nodeName    :: NodeName
                         , _nodeDevices :: [(DeviceName, DeviceRep)]
                         }
  deriving (Generic, Binary, Typeable, Show)
makeLenses ''NodeInfo

data Status = Status { _connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Binary, Typeable, Show)
makeLenses ''Status
