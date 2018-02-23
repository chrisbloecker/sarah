{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device
  ( IsDevice (..)
  , Device (..)
  , DeviceRep (unDeviceRep)
  , toDeviceRep

  , DHT22
  , ExampleDevice
  , HS110
  , ToshibaAC
  ) where
--------------------------------------------------------------------------------
import Control.Applicative                  ((<|>))
import Data.Aeson                           (ToJSON (..), FromJSON (..), encode, decode', eitherDecode')
import Data.Aeson.Types                     (Parser)
import Data.Binary                          (Binary)
import Data.ByteString.Lazy                 (ByteString, toStrict, fromStrict)
import Data.Text                            (Text)
import Data.Text.Encoding                   (encodeUtf8, decodeUtf8)
import Network.WebSockets                   (WebSocketsData (..))
import Sarah.Middleware.Model               (IsDevice (..), verboseFromJust)
--------------------------------------------------------------------------------
-- The devices, we need to enumerate them manually in the FromJSON instance for Device
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

-- Device representations contain a JSON string that is converted to a text, so
-- we can encode a device representation itself to JSON and send it over websockets.
-- ToDo: there must be a better, more straigtforward way to do this...
newtype DeviceRep = DeviceRep { unDeviceRep :: Text } deriving (Show, Binary, ToJSON, FromJSON)

toDeviceRep :: Device -> DeviceRep
toDeviceRep = DeviceRep . decodeUtf8 . toStrict . encode

fromDeviceRep :: DeviceRep -> Device
fromDeviceRep = verboseFromJust "fromJust failed in fromDeviceRep" . decode' . fromStrict . encodeUtf8 . unDeviceRep
