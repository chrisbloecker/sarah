{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device
  where
--------------------------------------------------------------------------------
import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Data.Aeson                       (encode, decode')
import           Data.Aeson.Types                 (Parser, Value (..), typeMismatch)
import           Data.ByteString.Lazy             (ByteString)
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Data.Text.Encoding               (encodeUtf8, decodeUtf8)
import           Data.Word
import           GHC.Generics                     (Generic)
import           Import.DeriveJSON
import           Import.MkBinary
import           Raspberry.Hardware
import           Sarah.Middleware.Model.Interface
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy                 as BS
import qualified Sarah.Middleware.Device.AC.Toshiba   as Toshiba
import qualified Sarah.Middleware.Device.Sensor.DHT22 as DHT22
import qualified Sarah.Persist.Types                  as T
--------------------------------------------------------------------------------

data DeviceCommand     = DeviceCommand
data DeviceQuery       = DeviceQuery
data DeviceQueryResult = DeviceQueryResult

deriveJSON jsonOptions ''DeviceCommand
deriveJSON jsonOptions ''DeviceQuery
deriveJSON jsonOptions ''DeviceQueryResult

class IsDevice model

class (IsDevice model, IsInterface interface) => HasBackend model interface where
  startDeviceController :: model -> Process ProcessId

class IsDevice model => IsAC model where
  type State model :: *

class IsDevice model => IsSensor model
class IsDevice model => IsTV     model

--------------------------------------------------------------------------------

data DHT22 = DHT22 deriving (Show)
instance IsDevice DHT22
instance ToJSON DHT22 where
  toJSON DHT22 = "DHT22"
instance FromJSON DHT22 where
  parseJSON (String "DHT22") = return DHT22
  parseJSON invalid          = typeMismatch "DHT22" invalid
instance IsSensor DHT22
instance HasBackend DHT22 GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice DHT22"

--------------------------------------------------------------------------------

data Toshiba_16NKV_E = Toshiba_16NKV_E deriving (Show)
instance IsDevice Toshiba_16NKV_E
instance ToJSON Toshiba_16NKV_E where
  toJSON Toshiba_16NKV_E = "Toshiba_16NKV_E"
instance FromJSON Toshiba_16NKV_E where
  parseJSON (String "Toshiba_16NKV_E") = return Toshiba_16NKV_E
  parseJSON invalid                    = typeMismatch "Toshiba_16NKV_E" invalid
instance HasBackend Toshiba_16NKV_E GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_16NKV_E"

--------------------------------------------------------------------------------

data Toshiba_RAS_M13NKCV = Toshiba_RAS_M13NKCV deriving (Show)
instance IsDevice Toshiba_RAS_M13NKCV
instance ToJSON Toshiba_RAS_M13NKCV where
  toJSON Toshiba_RAS_M13NKCV = "Toshiba_RAS_M13NKCV"
instance FromJSON Toshiba_RAS_M13NKCV where
  parseJSON (String "Toshiba_RAS_M13NKCV") = return Toshiba_RAS_M13NKCV
  parseJSON invalid                        = typeMismatch "Toshiba_RAS_M13NKCV" invalid
instance HasBackend Toshiba_RAS_M13NKCV GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_RAS_M13NKCV"

--------------------------------------------------------------------------------

data Toshiba_RAS_M16NKCV = Toshiba_RAS_M16NKCV deriving (Show)
instance IsDevice Toshiba_RAS_M16NKCV
instance ToJSON Toshiba_RAS_M16NKCV where
  toJSON Toshiba_RAS_M16NKCV = "Toshiba_RAS_M16NKCV"
instance FromJSON Toshiba_RAS_M16NKCV where
  parseJSON (String "Toshiba_RAS_M16NKCV") = return Toshiba_RAS_M16NKCV
  parseJSON invalid                        = typeMismatch "Toshiba_RAS_M16NKCV" invalid
instance HasBackend Toshiba_RAS_M16NKCV GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_RAS_M16NKCV"

--------------------------------------------------------------------------------

data Device = forall model. (IsDevice model, Show model, FromJSON model, ToJSON model) => Device model deriving (Typeable)
instance Show Device where
  show (Device model) = "Device " ++ show model
instance ToJSON Device where
  toJSON (Device model) = toJSON model
instance FromJSON Device where
  parseJSON v = Device <$> (parseJSON v :: Parser DHT22)
            <|> Device <$> (parseJSON v :: Parser Toshiba_16NKV_E)
            <|> Device <$> (parseJSON v :: Parser Toshiba_RAS_M13NKCV)
            <|> Device <$> (parseJSON v :: Parser Toshiba_RAS_M16NKCV)
            <|> fail ("Can't parse device from JSON: " ++ show v)

-- ToDo: Instead of this, we should probably rather write instances for Binary and Typeable for Device
newtype DeviceRep = DeviceRep { unDeviceRep :: Text } deriving (Show, Binary)
deriveJSON jsonOptions ''DeviceRep

toDeviceRep :: Device -> DeviceRep
toDeviceRep = DeviceRep . decodeUtf8 . BS.toStrict . encode

-- ToDo: This is not supposed to go wrong because DeviceReps can only be created
--       from valid Devices, so we will be able to decode them again
fromDeviceRep :: DeviceRep -> Device
fromDeviceRep = fromJust . decode' . BS.fromStrict . encodeUtf8 . unDeviceRep

--------------------------------------------------------------------------------
{-
(+++) = append

-- ToDo: carry around the current state somewhere
toshibaServer :: NodeName -> Master -> Device -> Process ()
toshibaServer nodeName master device = case device^.deviceInterface of
  GPIO pin -> do
    _config <- expect
    liftIO $ Toshiba.send pin _config
    toshibaServer nodeName master device


dht22Server :: NodeName -> Master -> Device -> T.Room -> Process ()
dht22Server nodeName master device room = case device^.deviceInterface of
  GPIO pin -> do
    mReadings <- liftIO $ do secs <- flip mod 60 . fromIntegral . sec <$> getTime Realtime
                             threadDelay (1000000 * (60 - secs))
                             DHT22.get pin
    case mReadings of
      Left err -> do
        -- ToDo: fiddle the node name in
        let message = "Reading " +++ (pack .show $ device^.deviceName) +++ " failed: " +++ (pack . show $ err)
        say $ unpack message
        sendMaster master $ Log nodeName message T.Error

      Right (Temperature t, Humidity h) -> do
        sendMaster master $ SensorReading room T.Temperature t
        sendMaster master $ SensorReading room T.Humidity    h

    dht22Server nodeName master device room
-}
