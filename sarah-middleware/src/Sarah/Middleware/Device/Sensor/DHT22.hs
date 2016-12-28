{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import           Raspberry.GPIO
--------------------------------------------------------------------------------
import qualified Language.C.Inline as C
--------------------------------------------------------------------------------

newtype Temperature = Temperature { unTemperature :: Float } deriving (Show)
newtype Humidity    = Humidity    { unHumidity    :: Float } deriving (Show)

--------------------------------------------------------------------------------

C.context C.baseCtx
C.include "<stdio.h>"
C.include "dht22.h"

get :: Pin -> IO (Temperature, Humidity)
get (Pin pin) = do
  _ <- [C.block| void
         {
           float humidity
               , temperature
               ;
           readDHT22(4, &humidity, &temperature);
           fprintf(stderr, "temperature: %f, humidity: %f", temperature, humidity);
         }
       |]
  return (Temperature 42, Humidity 80)
