{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Data.Text                   (unpack)
import Data.Aeson                  (Value (..))
import Data.Typeable               (Typeable)
import Import.DeriveJSON
import Physics
import Raspberry.GPIO
import Sarah.Middleware.Model
--------------------------------------------------------------------------------
import qualified Language.C.Inline            as C
import qualified Data.Vector.Storable.Mutable as V
--------------------------------------------------------------------------------

data DHT22 = DHT22 Pin deriving (Show)

instance IsDevice DHT22 where
  type DeviceState DHT22 = ()
  startDeviceController (DHT22 pin) portManager = do
    say $ "[DHT22.startDeviceController]"
    DeviceController <$> spawnLocal (controller portManager pin)

      where
        controller :: PortManager -> Pin -> Process ()
        controller portManager pin = receiveWait [ matchAny $ \m -> do
                                                     say $ "[DHT22] Received unexpected message" ++ show m
                                                     controller portManager pin
                                                 ]

instance ToJSON DHT22 where
  toJSON (DHT22 (Pin pin)) = object [ "model" .= String "DHT22"
                                    , "gpio"  .= toJSON pin
                                    ]

instance FromJSON DHT22 where
  parseJSON = withObject "DHT22" $ \o -> do
    model <- o .: "model" :: Parser Text
    case model of
      "DHT22" -> DHT22 <$> (Pin <$> o .: "gpio")
      model   -> fail $ "Invalid model identifier: " ++ unpack model

data Error = InitFailed
           | Timeout
           | Parameter
           | Unknown
  deriving (Show)

toError :: C.CInt -> Error
toError 1 = InitFailed
toError 2 = Timeout
toError 3 = Parameter
toError _ = Unknown

--------------------------------------------------------------------------------

C.context C.baseCtx
C.include "<stdio.h>"
C.include "dht22.h"

get :: Pin -> IO (Either Error (Temperature, Humidity))
get (Pin pin) = do
  vec <- V.new 2
  res <- V.unsafeWith vec $ \ptr ->
    [C.block| int
    {
      int retry = 0;
      int res   = 0;

      while (retry++ < 10 && (res = readDHT22(4, &$(double * ptr)[0], &$(double * ptr)[1])));

      return res;
    }
    |]
  humidity    <- V.read vec 0
  temperature <- V.read vec 1
  return $ if res == 0 then Right ( Temperature (unCDouble temperature)
                                  , Humidity    (unCDouble humidity)
                                  )
                       else Left . toError $ res


unCDouble :: C.CDouble -> Double
unCDouble (C.CDouble d) = d
