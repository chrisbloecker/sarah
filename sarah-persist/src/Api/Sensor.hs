{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Api.Sensor
  where
--------------------------------------------------------------------------------
import           Config
import           Data.Time.Calendar       (Day)
import           Database.Persist
import           Model
import           Servant
import           Servant.JS               (vanillaJS, writeJSForAPI)
--------------------------------------------------------------------------------

type SensorApi = "sensor-readings" :> "date"   :> Capture "date"   Day
                                   :> "room"   :> Capture "room"   Room
                                   :> "sensor" :> Capture "sensor" Sensor
                                   :> Get     '[JSON] [Entity SensorReading]
            :<|> "sensor-readings" :> ReqBody '[JSON] SensorReading
                                   :> Put     '[JSON] ()

--------------------------------------------------------------------------------

sensorServer :: ServerT SensorApi AppM
sensorServer = getSensorReadings
          :<|> putSensorReading

getSensorReadings :: Day -> Room -> Sensor -> AppM [Entity SensorReading]
getSensorReadings day room sensor =
  runDb $ selectList [ SensorReadingDate   ==. day
                     , SensorReadingRoom   ==. room
                     , SensorReadingSensor ==. sensor
                     ]
                     []

putSensorReading :: SensorReading -> AppM ()
putSensorReading sensorReading = do
  _ <- runDb (insert sensorReading)
  return ()

generateJavaScript :: IO ()
generateJavaScript = writeJSForAPI (Proxy :: Proxy SensorApi) vanillaJS "./assets/api.js"
