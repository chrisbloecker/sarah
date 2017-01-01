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

setupDevice :: Master -> T.Room -> Device -> IO ProcessId
setupDevice master room device = do
  let node = undefined
  forkProcess node $ case device^.deviceModel of
    AC     Toshiba_RAS_M13NKCV -> toshibaServer master device
    AC     Toshiba_RAS_M16NKCV -> toshibaServer master device
    AC     Toshiba_16NKV_E     -> toshibaServer master device
    Sensor DHT22               -> dht22Server   master device room
    model                      -> dummyServer   master device


dummyServer :: Master -> Device -> Process ()
dummyServer master device =
  -- ToDo: fiddle the node name in
  sendMaster master $ Log "NodeName" ("Dummy server starterd for " +++ (device^.deviceName)) T.Debug


toshibaServer :: Master -> Device -> Process ()
toshibaServer master device = case device^.deviceInterface of
  GPIO pin -> do
    _config <- expect
    liftIO $ Toshiba.send pin _config
    toshibaServer master device


dht22Server :: Master -> Device -> T.Room -> Process ()
dht22Server master device room = case device^.deviceInterface of
  GPIO pin -> do
    liftIO $ threadDelay =<< flip mod 60 . fromIntegral . sec <$> getTime Realtime
    mReadings <- liftIO $ DHT22.get pin
    case mReadings of
      Left err ->
        -- ToDo: fiddle the node name in
        sendMaster master $ Log "NoneName" ("Reading " +++ (device^.deviceName) +++ " failed: " +++ (pack . show $ err)) T.Error

      Right (Temperature t, Humidity h) -> do
        sendMaster master $ SensorReading room T.Temperature t
        sendMaster master $ SensorReading room T.Humidity    h
        dht22Server master device room
