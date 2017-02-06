{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
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
import           Control.Lens
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

class IsInterface interface => IsDevice model interface | model -> interface where
  getInterface :: model -> interface
  startDeviceController :: model -> Process ProcessId

class IsDevice model interface => IsAC model interface where
  type State model :: *

class IsDevice model interface => IsSensor model interface
class IsDevice model interface => IsTV     model interface

data Device = forall model interface. (IsDevice model interface) => Device model

--instance IsInterface interface => IsDevice Device interface where
--  getInterface (Device model) = getInterface model
--  startDeviceController = error "startDeviceController not implemented for instance IsDevice Device interface"

instance ToJSON Device where
  toJSON (Device model) = toJSON ("Device" :: String)

instance FromJSON Device where
  parseJSON = undefined

data DHT22 = DHT22 GPIO
instance IsDevice DHT22 GPIO where
  getInterface (DHT22 gpio) = gpio
  startDeviceController = error "startDeviceController not implemented for instance IsDevice DHT22"
instance IsSensor DHT22 GPIO where
deriveJSON jsonOptions ''DHT22

--------------------------------------------------------------------------------

data Toshiba_16NKV_E = Toshiba_16NKV_E GPIO
instance IsDevice Toshiba_16NKV_E GPIO where
  getInterface (Toshiba_16NKV_E gpio) = gpio
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_16NKV_E"
instance IsAC Toshiba_16NKV_E GPIO where
  type State Toshiba_16NKV_E = Toshiba.Config
deriveJSON jsonOptions ''Toshiba_16NKV_E

--------------------------------------------------------------------------------

newtype Toshiba_RAS_M13NKCV = Toshiba_RAS_M13NKCV GPIO
instance IsDevice Toshiba_RAS_M13NKCV GPIO where
  getInterface (Toshiba_RAS_M13NKCV gpio) = gpio
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_RAS_M13NKCV"
instance IsAC Toshiba_RAS_M13NKCV GPIO where
  type State Toshiba_RAS_M13NKCV = Toshiba.Config
deriveJSON jsonOptions ''Toshiba_RAS_M13NKCV

--------------------------------------------------------------------------------

newtype Toshiba_RAS_M16NKCV = Toshiba_RAS_M16NKCV GPIO
instance IsDevice Toshiba_RAS_M16NKCV GPIO where
  getInterface (Toshiba_RAS_M16NKCV gpio) = gpio
  startDeviceController = error "startDeviceController not implemented for instance IsDevice Model_Toshiba_RAS_M16NKCV"
instance IsAC Toshiba_RAS_M16NKCV GPIO where
  type State Toshiba_RAS_M16NKCV = Toshiba.Config
deriveJSON jsonOptions ''Toshiba_RAS_M16NKCV

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
