{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master.Messages
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process              (Process, ProcessId, send)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Aeson                               (ToJSON (..), FromJSON (..), encode, decode')
import Data.Aeson.Types                         (Parser)
import Data.Binary                              (Binary)
import Data.ByteString.Lazy                     (ByteString)
import Data.Constraint
import Data.Maybe                               (fromJust)
import Data.Text                                (Text)
import Data.Typeable                            (Typeable)
import GHC.Generics                             (Generic)
import Network.WebSockets                       (WebSocketsData (..))
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed
import Sarah.Middleware.Model
import Sarah.Persist.Model
--------------------------------------------------------------------------------

class ( Binary (MRequest command), Generic (MRequest command), Typeable (MRequest command), WebSocketsData (MRequest command), ToJSON (MRequest command), FromJSON (MRequest command)
      , Binary (MReply   command), Generic (MReply   command), Typeable (MReply   command), WebSocketsData (MReply   command), ToJSON (MReply   command), FromJSON (MReply   command)
      ) => IsMasterCommand command where
  data MRequest command :: *
  data MReply   command :: *

data GetStatus

instance IsMasterCommand GetStatus where
  data MRequest GetStatus = GetStatusRequest         deriving (Binary, Generic, Typeable, ToJSON, FromJSON)
  data MReply   GetStatus = GetStatusReply   Status  deriving (Binary, Generic, Typeable, ToJSON, FromJSON)

instance WebSocketsData (MRequest GetStatus) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

instance WebSocketsData (MReply GetStatus) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'


data Log = Log Text Text LogLevel
  deriving (Binary, Generic, Typeable)

data NodeUp = NodeUp ProcessId NodeInfo
  deriving (Binary, Generic, Typeable)

data DeviceStateChanged = DeviceStateChanged DeviceAddress EncodedDeviceState
  deriving (Binary, Generic, Typeable)

-- ToDo: how to sore sensor readings in general? There could be "fuzzy sensors"
--       that don't return numerical readings, but something weird
data SensorReading = SensorReading Room Sensor Double deriving (Binary, Generic, Typeable)

deriving instance Binary Room
deriving instance Binary Sensor
deriving instance Binary LogLevel

data MasterRequest = forall command. (IsMasterCommand command)
                   => MasterRequest (MRequest command)

instance ToJSON MasterRequest where
  toJSON (MasterRequest request) = toJSON request

instance FromJSON MasterRequest where
  parseJSON v = MasterRequest <$> (parseJSON v :: Parser (MRequest GetStatus))

instance WebSocketsData MasterRequest where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

mkMasterRequest :: IsMasterCommand command => MRequest command -> MasterRequest
mkMasterRequest = MasterRequest

--------------------------------------------------------------------------------

nodeUp :: Master -> ProcessId -> NodeInfo -> Process ()
nodeUp master pid nodeInfo = send (unMaster master) (NodeUp pid nodeInfo)

getStatus :: Master -> ProcessId -> Process ()
getStatus master pid = send (unMaster master) (FromPid pid GetStatusRequest)

sendMaster :: Serializable a => Master -> a -> Process ()
sendMaster master = send (unMaster master)
