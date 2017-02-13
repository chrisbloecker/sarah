{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process
import           Physics
import           Raspberry.GPIO
--------------------------------------------------------------------------------
import qualified Language.C.Inline            as C
import qualified Data.Vector.Storable.Mutable as V
--------------------------------------------------------------------------------

controller :: ProcessId -> Process ()
controller interface = receiveWait [ matchAny $ \m -> do
                                       say $ "Received unexpected message" ++ show m
                                       controller interface
                                   ]

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
