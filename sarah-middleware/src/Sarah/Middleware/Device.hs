{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device
  where
--------------------------------------------------------------------------------
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Lens                     hiding ((.=))
-- ToDo: why do I need this import to get Data.Aeson.Object into scope?
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text
import           GHC.Generics                     (Generic)
import           Import.DeriveJSON
import           Physics
import           Raspberry.GPIO
import           Sarah.Middleware.Model
import           Sarah.Middleware.Master.Messages
import           System.Clock
--------------------------------------------------------------------------------
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

data Device = forall model interface. (HasBackend model interface, FromJSON model, ToJSON model, FromJSON interface, ToJSON interface) => Device model interface

instance ToJSON Device where
  toJSON (Device model interface) = object [ "model"     .= toJSON model
                                           , "interface" .= toJSON interface
                                           ]

data DHT22 = DHT22 deriving (Show)
instance FromJSON DHT22 where
  parseJSON (String "DHT22") = return DHT22
  parseJSON invalid          = typeMismatch "DHT22" invalid
instance ToJSON DHT22 where toJSON DHT22 = "DHT22"
instance IsDevice DHT22
instance IsSensor DHT22
instance HasBackend DHT22 GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice DHT22"


--------------------------------------------------------------------------------

data Toshiba_16NKV_E = Toshiba_16NKV_E deriving (Show)
instance FromJSON Toshiba_16NKV_E where
  parseJSON (String "Toshiba_16NKV_E") = return Toshiba_16NKV_E
  parseJSON invalid                    = typeMismatch "Toshiba_16NKV_E" invalid
instance ToJSON Toshiba_16NKV_E where toJSON Toshiba_16NKV_E = "Toshiba_16NKV_E"
instance IsDevice Toshiba_16NKV_E
instance HasBackend Toshiba_16NKV_E GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_16NKV_E"

--------------------------------------------------------------------------------
{-
data Toshiba_RAS_M13NKCV = Toshiba_RAS_M13NKCV
instance IsDevice Toshiba_RAS_M13NKCV
instance HasBackend Toshiba_RAS_M13NKCV GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_RAS_M13NKCV"
deriveJSON jsonOptions ''Toshiba_RAS_M13NKCV

--------------------------------------------------------------------------------

data Toshiba_RAS_M16NKCV = Toshiba_RAS_M16NKCV
instance IsDevice Toshiba_RAS_M16NKCV
instance HasBackend Toshiba_RAS_M16NKCV GPIO where
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_RAS_M16NKCV"
deriveJSON jsonOptions ''Toshiba_RAS_M16NKCV
-}
--------------------------------------------------------------------------------
{-
convert :: DeviceModel -> Device
convert Model_Toshiba_16NKV_E     = Device $ AC $ Toshiba_16NKV_E (GPIO undefined)
convert Model_Toshiba_RAS_M13NKCV = Device $ AC $ Toshiba_RAS_M13NKCV (GPIO undefined)
convert Model_Toshiba_RAS_M16NKCV = Device $ AC $ Toshiba_RAS_M16NKCV (GPIO undefined)
convert Model_DHT22               = Device $ Sensor $ DHT22 (GPIO undefined)
-}
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
