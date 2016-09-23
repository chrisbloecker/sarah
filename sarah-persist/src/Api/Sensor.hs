{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Api.Sensor
  where
--------------------------------------------------------------------------------
import Data.Time.Calendar         (Day)
import Servant
import Servant.JS                 (vanillaJS, writeJSForAPI)
--------------------------------------------------------------------------------
import Config
import Model
--------------------------------------------------------------------------------

type SensorApi = "sensor-readings" :> "date"   :> Capture "date"   Day
                                   :> "room"   :> Capture "room"   Room
                                   :> "sensor" :> Capture "sensor" Sensor
                                   :> Get     '[JSON] [SensorReading]
            :<|> "sensor-readings" :> ReqBody '[JSON] SensorReading
                                   :> Put     '[JSON] SensorReading

--------------------------------------------------------------------------------

sensorServer :: ServerT SensorApi AppM
sensorServer = getSensorReadings
          :<|> putSensorReading

--getSensorReadings :: Day -> Room -> Sensor -> AppM [SensorReading]
getSensorReadings day room sensor = undefined

--putSensorReading :: Handler
putSensorReading = undefined

generateJavaScript :: IO ()
generateJavaScript = writeJSForAPI (Proxy :: Proxy SensorApi) vanillaJS "./assets/api.js"
