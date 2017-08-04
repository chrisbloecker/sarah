{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Master.Messages
  where
--------------------------------------------------------------------------------
import Control.Applicative                      ((<|>))
import Control.Distributed.Process              (Process, ProcessId, send)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Aeson                               (ToJSON (..), FromJSON (..), encode, decode')
import Data.Aeson.Types                         (Parser, Value (..), (.=), (.:), withObject, object)
import Data.Binary                              (Binary)
import Data.ByteString.Lazy                     (ByteString)
import Data.Constraint
import Data.Maybe                               (fromJust)
import Data.Text                                (Text, unpack)
import Data.Typeable                            (Typeable)
import GHC.Generics                             (Generic)
import Network.WebSockets                       (WebSocketsData (..))
import Sarah.Middleware.Database                (Dimension, Log (..), LogLevel, Schedule (..))
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed
import Sarah.Middleware.Model
--------------------------------------------------------------------------------

class ( Binary (MRequest command), Generic (MRequest command), Typeable (MRequest command), WebSocketsData (MRequest command), ToJSON (MRequest command), FromJSON (MRequest command)
      , Binary (MReply   command), Generic (MReply   command), Typeable (MReply   command), WebSocketsData (MReply   command), ToJSON (MReply   command), FromJSON (MReply   command)
      ) => IsMasterCommand command where
  data MRequest command :: *
  data MReply   command :: *


-- | for getting the current status
data GetStatus

instance IsMasterCommand GetStatus where
  data MRequest GetStatus = GetStatusRequest         deriving (Binary, Generic, Typeable)
  data MReply   GetStatus = GetStatusReply   Status  deriving (Binary, Generic, Typeable)

instance ToJSON (MRequest GetStatus) where
 toJSON GetStatusRequest = object [ "request" .= String "GetStatusRequest" ]

instance FromJSON (MRequest GetStatus) where
  parseJSON = withObject "MRequest GetStatus" $ \o -> do
    request <- o .: "request" :: Parser Text
    case request of
      "GetStatusRequest" -> return GetStatusRequest
      request            -> fail $ "Invalid request: " ++ unpack request

instance ToJSON (MReply GetStatus) where
  toJSON (GetStatusReply status) = object [ "reply"  .= String "GetStatusReply"
                                          , "status" .= toJSON status
                                          ]

instance FromJSON (MReply GetStatus) where
  parseJSON = withObject "MReply GetStatus" $ \o -> do
    reply <- o .: "reply" :: Parser Text
    case reply of
      "GetStatusReply" -> GetStatusReply <$> o .: "status"
      reply            -> fail $ "Invalid reply: " ++ unpack reply

instance WebSocketsData (MRequest GetStatus) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

instance WebSocketsData (MReply GetStatus) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'


-- | for getting the schedule
data GetSchedule

deriving instance Generic Schedule
deriving instance Binary  Schedule

instance IsMasterCommand GetSchedule where
  data MRequest GetSchedule = GetScheduleRequest            deriving (Binary, Generic, Typeable)
  data MReply   GetSchedule = GetScheduleReply   [Schedule] deriving (Binary, Generic, Typeable)

instance ToJSON (MRequest GetSchedule) where
  toJSON GetScheduleRequest = object [ "request" .= String "GetScheduleRequest" ]

instance FromJSON (MRequest GetSchedule) where
  parseJSON = withObject "MRequest GetSchedule" $ \o -> do
    request <- o .: "request" :: Parser Text
    case request of
      "GetScheduleRequest" -> return GetScheduleRequest
      request              -> fail $ "Invalid request: " ++ unpack request

instance ToJSON (MReply GetSchedule) where
  toJSON (GetScheduleReply schedule) = object [ "reply"    .= String "GetScheduleReply"
                                              , "schedule" .= toJSON schedule
                                              ]

instance FromJSON (MReply GetSchedule) where
  parseJSON = withObject "MReply GetSchedule" $ \o -> do
    reply <- o .: "reply" :: Parser Text
    case reply of
      "GetScheduleReply" -> GetScheduleReply <$> o .: "schedule"
      reply              -> fail $ "Invalid reply: " ++ unpack reply

instance WebSocketsData (MRequest GetSchedule) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

instance WebSocketsData (MReply GetSchedule) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'


-- | for getting the logs
data GetLogs

deriving instance Generic Log
deriving instance Binary  Log

instance IsMasterCommand GetLogs where
  data MRequest GetLogs = GetLogsRequest       deriving (Binary, Generic, Typeable, Show)
  data MReply   GetLogs = GetLogsReply   [Log] deriving (Binary, Generic, Typeable, Show)

instance ToJSON (MRequest GetLogs) where
  toJSON GetLogsRequest = object [ "request" .= String "GetLogsRequest" ]

instance FromJSON (MRequest GetLogs) where
  parseJSON = withObject "MRequest GetLogs" $ \o -> do
    request <- o .: "request" :: Parser Text
    case request of
      "GetLogsRequest" -> return GetLogsRequest
      request          -> fail $ "Invalid request: " ++ unpack request

instance ToJSON (MReply GetLogs) where
  toJSON (GetLogsReply logs) = object [ "reply" .= String "GetLogsReply"
                                      , "logs"  .= toJSON logs
                                      ]

instance FromJSON (MReply GetLogs) where
  parseJSON = withObject "MReply Getlogs" $ \o -> do
    reply <- o .: "reply" :: Parser Text
    case reply of
      "GetLogsReply" -> GetLogsReply <$> o .: "logs"
      reply          -> fail $ "Invalid reply: " ++ unpack reply

instance WebSocketsData (MRequest GetLogs) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

instance WebSocketsData (MReply GetLogs) where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'



data PutLog = PutLog Text Text LogLevel
  deriving (Binary, Generic, Typeable)

data NodeUp = NodeUp ProcessId NodeInfo
  deriving (Binary, Generic, Typeable)

data DeviceStateChanged = DeviceStateChanged DeviceAddress EncodedDeviceState
  deriving (Binary, Generic, Typeable)

-- ToDo: how to sore sensor readings in general? There could be "fuzzy sensors"
--       that don't return numerical readings, but something weird
data LogSensorReading = LogSensorReading Room DeviceAddress Dimension deriving (Binary, Generic, Typeable)


data MasterRequest = forall command. (IsMasterCommand command)
                   => MasterRequest (MRequest command)

instance ToJSON MasterRequest where
  toJSON (MasterRequest request) = toJSON request

instance FromJSON MasterRequest where
  parseJSON v = MasterRequest <$> (parseJSON v :: Parser (MRequest GetStatus))
            <|> MasterRequest <$> (parseJSON v :: Parser (MRequest GetSchedule))

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
