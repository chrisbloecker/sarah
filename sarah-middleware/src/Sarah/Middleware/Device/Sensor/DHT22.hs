{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import           Raspberry.GPIO
--------------------------------------------------------------------------------
import qualified Language.C.Inline            as C
import qualified Data.Vector.Storable.Mutable as V
--------------------------------------------------------------------------------

newtype Temperature = Temperature { unTemperature :: Double } deriving (Show)
newtype Humidity    = Humidity    { unHumidity    :: Double } deriving (Show)

data Error = INIT_FAILED
           | TIMEOUT
           | UNKNOWN
  deriving (Show)

toError :: C.CInt -> Error
toError 1 = INIT_FAILED
toError 2 = TIMEOUT
toError _ = UNKNOWN

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
