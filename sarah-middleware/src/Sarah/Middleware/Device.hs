{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device
  ( IsDevice (..)
  , Device (..)
  , DeviceRep (..), toDeviceRep
  , DHT22     -- temperature and humidity sensor
  , HS110     -- TPLink smart plug
  , ToshibaAC -- well, guess...
  ) where
--------------------------------------------------------------------------------
import Control.Applicative              ((<|>))
import Control.Concurrent               (threadDelay)
import Control.Distributed.Process      (Process, ProcessId, say, spawnLocal, matchAny, receiveWait, liftIO)
import Data.Aeson                       (encode, decode')
import Data.Aeson.Types                 (Parser, Value (..), typeMismatch)
import Data.Maybe                       (fromJust)
import Data.Text                        (Text, unpack)
import Data.Text.Encoding               (encodeUtf8, decodeUtf8)
import Import.DeriveJSON
import Import.MkBinary
import Raspberry.Hardware
import Sarah.Middleware.Model               (IsDevice)
import Sarah.Middleware.Model.Interface
import Sarah.Middleware.Device.AC.Toshiba   (ToshibaAC)
import Sarah.Middleware.Device.Power.HS110  (HS110)
import Sarah.Middleware.Device.Sensor.DHT22 (DHT22)
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
import qualified Sarah.Persist.Types  as T
--------------------------------------------------------------------------------

-- A Device is a wrapper around something that is an instance of IsDevice. We're
-- forgetting almost all type information here, and perhaps it would be better
-- to model devices as something like
-- data Device = DHT22 | ToshibaAC | ...
-- but then we would have a lot of case alternatives everywhere where we want to
-- use devices and have to handle different models differently. So we're taking
-- the approach of existentials and see how that goes.
data Device = forall model. (IsDevice model, Show model, FromJSON model, ToJSON model)
            => Device model

instance Show Device where
  show (Device model) = "Device (" ++ show model ++ ")"

instance ToJSON Device where
  toJSON (Device model) = toJSON model

-- We have to explicitly list the devices we want to be able to parse because
-- there are existentials in the context of Device and the FromJSON instance
-- can't be derived automatically.
instance FromJSON Device where
  parseJSON v = Device <$> (parseJSON v :: Parser DHT22)
            <|> Device <$> (parseJSON v :: Parser HS110)
            <|> Device <$> (parseJSON v :: Parser ToshibaAC)
            <|> fail ("Can't parse Device from JSON: " ++ show v)

-- We're representing devices using JSON. DeviceReps can be serialised and sent
-- over the network.
newtype DeviceRep = DeviceRep { unDeviceRep :: Text } deriving (Show, Binary)
deriveJSON jsonOptions ''DeviceRep

toDeviceRep :: Device -> DeviceRep
toDeviceRep = DeviceRep . decodeUtf8 . BS.toStrict . encode

-- This is not supposed to go wrong because DeviceReps *should only* be created
-- from valid Devices, so we will be able to decode them again
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
