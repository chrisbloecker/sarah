{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Sarah.Middleware.Distributed
  where

import Control.Lens
import Data.Text               (Text)
import Import.MkBinary
import Import.DeriveJSON
import Sarah.Middleware.Device (DeviceRep)
import Sarah.Middleware.Model  (DeviceName)

type NodeName = Text

data NodeInfo = NodeInfo { _nodeName    :: NodeName
                         , _nodeDevices :: [(DeviceName, DeviceRep)]
                         }
  deriving (Generic, Typeable, Show)
instance Binary NodeInfo
deriveJSON jsonOptions ''NodeInfo
makeLenses ''NodeInfo

data Status = Status { _connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Typeable, Show)
instance Binary Status
deriveJSON jsonOptions ''Status
makeLenses ''Status
