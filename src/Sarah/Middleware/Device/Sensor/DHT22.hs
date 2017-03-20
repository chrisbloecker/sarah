{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Monad                (void)
import Control.Monad.Reader         (ReaderT, runReaderT, ask, lift)
import Data.Text                    (Text, pack, unpack)
import Data.Aeson                   (ToJSON (..), FromJSON (..), encode)
import Data.Aeson.Types             (Parser, Value (..), (.=), (.:), withObject, object)
import GHC.Generics                 (Generic)
import Physics
import Raspberry.GPIO
import Sarah.Middleware.Model
import Sarah.Middleware.Slave.Messages
import System.Clock
--------------------------------------------------------------------------------
import qualified Language.C.Inline            as C
import qualified Data.Vector.Storable.Mutable as V
--------------------------------------------------------------------------------

-- Haskell representation of the error codes listed in dht22.h
data Error = InitFailed
           | Timeout
           | Parameter
           | Unknown
  deriving (Show, Generic, ToJSON, FromJSON)

-- Conversion from C error codes to Haskell representation
toError :: C.CInt -> Error
toError 1 = InitFailed
toError 2 = Timeout
toError 3 = Parameter
toError _ = Unknown

--------------------------------------------------------------------------------

C.context C.baseCtx
C.include "<stdio.h>"
C.include "dht22.h"

readDHT22 :: Pin -> IO (Either Error (Temperature, Humidity))
readDHT22 (Pin pin) = do
  vec <- V.new 2
  res <- V.unsafeWith vec $ \ptr ->
    [C.block| int
    {
      int retry = 0;
      int res   = 0;

      while (retry++ < 3 && (res = readDHT22(4, &$(double * ptr)[0], &$(double * ptr)[1])));

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

--------------------------------------------------------------------------------

-- DHT22 sensors are connected through a GPIO pin.
newtype DHT22 = DHT22 Pin deriving (Show)

data ControllerEnv = ControllerEnv { slave       :: Slave
                                   , portManager :: PortManager
                                   , pin         :: Pin
                                   }

initState :: ControllerEnv -> Process DHT22State
initState ControllerEnv{..} = do
  ereadings <- liftIO $ readDHT22 pin
  readAt    <- liftIO $ getTime Monotonic
  case ereadings of
    Left dht22Error -> say $ "[DHT22.initState] Error reading sensor: " ++ show dht22Error
    Right _         -> return ()

  return $ DHT22State (SensorState ereadings) readAt

data DHT22State = DHT22State { sensorState :: DeviceState DHT22
                             , readAt      :: TimeSpec
                             }

instance IsDevice DHT22 where
  -- The DHT22 does not have a state
  data DeviceState DHT22 = SensorState { readings :: Either Error (Temperature, Humidity) }
    deriving (Generic, ToJSON, FromJSON)

  -- The DHT22 can be used to read the temperature, the humidity, or both
  data DeviceCommand DHT22 = GetReadings
    deriving (Generic, ToJSON, FromJSON)

  startDeviceController (DHT22 pin) slave portManager = do
    say "[DHT22.startDeviceController]"
    let env = ControllerEnv{..}
    DeviceController <$> spawnLocal (initState env >>= controller env)

      where
        controller :: ControllerEnv -> DHT22State -> Process ()
        controller env@ControllerEnv{..} state@DHT22State{..} =
          receiveWait [ match $ \(FromPid src Query{..}) ->
                          case (getCommand queryCommand :: Either String (DeviceCommand DHT22)) of
                            Left err -> say $ "[DHT22.controller] Can't decode command: " ++ err
                            Right command -> case command of
                              GetReadings -> do
                                -- ToDo: reserve the port through the port manager
                                now <- liftIO $ getTime Monotonic

                                -- if it's less than 2 seconds ago that we have read the sensor,
                                -- use the "cached" value, otherwise get a new reading
                                if sec (diffTimeSpec readAt now) < 2
                                  then do
                                    say "[DHT22.controller] Using cached readings"
                                    case readings sensorState of
                                      Left dht22Error -> send src $ mkError (pack . show $ dht22Error)
                                      Right readings  -> send src $ mkSuccess (encode readings)
                                    controller env state

                                  else do
                                    say "[DHT22.controller] Getting new readings"
                                    ereadings <- liftIO $ readDHT22 pin
                                    readingCompleted <- liftIO $ getTime Monotonic
                                    case ereadings of
                                      Left dht22Error -> do
                                        say $ "[DHT22.controller] Error reading sensor: " ++ show dht22Error
                                        send src $ mkError (pack . show $ dht22Error)
                                      Right readings ->
                                        send src $ mkSuccess (encode readings)

                                    sendStateChanged slave (SensorState ereadings)
                                    controller env state { sensorState = SensorState ereadings
                                                         , readAt      = readingCompleted
                                                         }

                      , matchAny $ \m -> do
                          say $ "[DHT22.controller] Received unexpected message" ++ show m
                          controller env state
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
