{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device
  where
--------------------------------------------------------------------------------
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Lens
import           Data.Text
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

(+++) = append

{-
setupDevice :: NodeName -> Master -> T.Room -> Device -> Process ProcessId
setupDevice nodeName master room device =
  spawnLocal $ case device^.deviceModel of
    AC     Toshiba_RAS_M13NKCV -> toshibaServer nodeName master device
    AC     Toshiba_RAS_M16NKCV -> toshibaServer nodeName master device
    AC     Toshiba_16NKV_E     -> toshibaServer nodeName master device
    Sensor DHT22               -> dht22Server   nodeName master device room
    model                      -> dummyServer   nodeName master device
-}

dummyServer :: NodeName -> Master -> Device -> Process ()
dummyServer nodeName master device =
  sendMaster master $ Log nodeName ("Dummy server starterd for " +++ (device^.deviceName)) T.Debug

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
