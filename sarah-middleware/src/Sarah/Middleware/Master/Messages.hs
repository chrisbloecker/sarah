{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master.Messages
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process              (Process, ProcessId, send)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Text                                (Text)
import Import.MkBinary
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed
import Sarah.Middleware.Model
import Sarah.Persist.Model
--------------------------------------------------------------------------------

data GetStatus     = GetStatus ProcessId              deriving (Binary, Generic, Typeable)
data Log           = Log Text Text LogLevel           deriving (Binary, Generic, Typeable)
data NodeUp        = NodeUp ProcessId NodeInfo        deriving (Binary, Generic, Typeable)
-- ToDo: some sensors may not read values that can be represented as Double
data SensorReading = SensorReading Room Sensor Double deriving (Binary, Generic, Typeable)

deriving instance Binary Room
deriving instance Binary Sensor
deriving instance Binary LogLevel

--------------------------------------------------------------------------------

nodeUp :: Master -> ProcessId -> NodeInfo -> Process ()
nodeUp (Master master) pid nodeInfo = send master (NodeUp pid nodeInfo)

getStatus :: Master -> ProcessId -> Process ()
getStatus (Master master) pid = send master (GetStatus pid)

sendMaster :: (Serializable a, Typeable a) => Master -> a -> Process ()
sendMaster (Master master) = send master
