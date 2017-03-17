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
import Data.Binary                              (Binary)
import Data.Text                                (Text)
import Data.Typeable                            (Typeable)
import GHC.Generics                             (Generic)
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed
import Sarah.Middleware.Model
import Sarah.Middleware.Types
import Sarah.Persist.Model
--------------------------------------------------------------------------------

data GetStatus          = GetStatus ProcessId                          deriving (Binary, Generic, Typeable)
data Log                = Log Text Text LogLevel                       deriving (Binary, Generic, Typeable)
data NodeUp             = NodeUp ProcessId NodeInfo                    deriving (Binary, Generic, Typeable)
data DeviceStateChanged = DeviceStateChanged DeviceAddress EncodedJSON deriving (Binary, Generic, Typeable)
-- ToDo: how to sore sensor readings in general? There could be "fuzzy sensors"
--       that don't return numerical readings, but something weird
data SensorReading = SensorReading Room Sensor Double deriving (Binary, Generic, Typeable)

deriving instance Binary Room
deriving instance Binary Sensor
deriving instance Binary LogLevel

--------------------------------------------------------------------------------

nodeUp :: Master -> ProcessId -> NodeInfo -> Process ()
nodeUp (Master master) pid nodeInfo = send master (NodeUp pid nodeInfo)

getStatus :: Master -> ProcessId -> Process ()
getStatus (Master master) pid = send master (GetStatus pid)

sendMaster :: Serializable a => Master -> a -> Process ()
sendMaster (Master master) = send master
