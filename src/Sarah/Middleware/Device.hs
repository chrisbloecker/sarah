{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device
  ( IsDevice (..)
  , Device (..)
  , DeviceRep (..), toDeviceRep
  , DHT22         -- temperature and humidity sensor
  , ExampleDevice -- the example device
  , HS110         -- TPLink smart plug
  , ToshibaAC     -- well, guess...
  ) where
--------------------------------------------------------------------------------
import Control.Applicative                  ((<|>))
import Data.Aeson                           (ToJSON (..), FromJSON (..), encode, decode')
import Data.Aeson.Types                     (Parser)
import Data.Binary                          (Binary)
import Data.Maybe                           (fromJust)
import Data.Text                            (Text)
import Sarah.Middleware.Model               (IsDevice)
import Sarah.Middleware.Types               (encodeAsText, decodeFromText)
--------------------------------------------------------------------------------
-- The devices
import Sarah.Middleware.Device.AC.Toshiba   (ToshibaAC)
import Sarah.Middleware.Device.Example      (ExampleDevice)
import Sarah.Middleware.Device.Power.HS110  (HS110)
import Sarah.Middleware.Device.Sensor.DHT22 (DHT22)
--------------------------------------------------------------------------------

-- A Device is a wrapper around something that is an instance of IsDevice. We're
-- forgetting almost all type information here, and perhaps it would be better
-- to model devices as something like
-- data Device = DHT22 | ToshibaAC | ...
-- but then we would have a lot of case alternatives everywhere where we want to
-- use devices and have to handle different models differently. So we're taking
-- the approach of existentials and see how that goes.
data Device = forall model. (IsDevice model, Show model)
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
            <|> Device <$> (parseJSON v :: Parser ExampleDevice)
            <|> fail ("Can't parse Device from JSON: " ++ show v)

-- We're representing devices using JSON. DeviceReps can be serialised and sent
-- over the network.
newtype DeviceRep = DeviceRep { unDeviceRep :: Text } deriving (Show, Binary, ToJSON, FromJSON)

toDeviceRep :: Device -> DeviceRep
toDeviceRep = DeviceRep . encodeAsText

-- This is not supposed to go wrong because DeviceReps *should only* be created
-- from valid Devices, so we will be able to decode them again
fromDeviceRep :: DeviceRep -> Device
fromDeviceRep = fromJust . decodeFromText . unDeviceRep
