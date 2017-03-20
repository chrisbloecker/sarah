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
import Data.Aeson                               (ToJSON (..), FromJSON (..), encode, decode)
import Data.Aeson.Types                         (Parser)
import Data.Binary                              (Binary)
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

class ( Binary (Request command), Generic (Request command), Typeable (Request command), WebSocketsData (Request command), ToJSON (Request command), FromJSON (Request command)
      , Binary (Reply   command), Generic (Reply   command), Typeable (Reply   command), WebSocketsData (Reply   command), ToJSON (Reply   command), FromJSON (Reply   command)
      ) => IsMasterCommand command where
  data Request command :: *
  data Reply   command :: *

data GetStatus

instance IsMasterCommand GetStatus where
  data Request GetStatus = GetStatusRequest         deriving (Binary, Generic, Typeable, ToJSON, FromJSON)
  data Reply   GetStatus = GetStatusReply   Status  deriving (Binary, Generic, Typeable, ToJSON, FromJSON)

instance WebSocketsData (Request GetStatus) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode

instance WebSocketsData (Reply GetStatus) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode


--data GetStatus          = GetStatus ProcessId                          deriving (Binary, Generic, Typeable)
data Log                = Log Text Text LogLevel                       deriving (Binary, Generic, Typeable)
data NodeUp             = NodeUp ProcessId NodeInfo                    deriving (Binary, Generic, Typeable)
data DeviceStateChanged = DeviceStateChanged DeviceAddress EncodedJSON deriving (Binary, Generic, Typeable)
-- ToDo: how to sore sensor readings in general? There could be "fuzzy sensors"
--       that don't return numerical readings, but something weird
data SensorReading = SensorReading Room Sensor Double deriving (Binary, Generic, Typeable)

deriving instance Binary Room
deriving instance Binary Sensor
deriving instance Binary LogLevel

data MasterRequest = forall command. (IsMasterCommand command)
                   => MasterRequest (Request command)

instance ToJSON MasterRequest where
  toJSON (MasterRequest request) = toJSON request

instance FromJSON MasterRequest where
  parseJSON v = MasterRequest <$> (parseJSON v :: Parser (Request GetStatus))


instance WebSocketsData MasterRequest where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode

mkMasterRequest :: IsMasterCommand command => Request command -> MasterRequest
mkMasterRequest = MasterRequest

--------------------------------------------------------------------------------

nodeUp :: Master -> ProcessId -> NodeInfo -> Process ()
nodeUp master pid nodeInfo = send (unMaster master) (NodeUp pid nodeInfo)

getStatus :: Master -> ProcessId -> Process ()
getStatus master pid = send (unMaster master) (FromPid pid GetStatusRequest)

sendMaster :: Serializable a => Master -> a -> Process ()
sendMaster master = send (unMaster master)
