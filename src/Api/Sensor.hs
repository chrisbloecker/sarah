{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Api.Sensor
  where
--------------------------------------------------------------------------------
import           Data.Time.Calendar       (Day)
import           Database.Persist
import           Model
import           Servant
import           Servant.Client
import           Types
--------------------------------------------------------------------------------

type SensorApi = "sensor-readings" :> "date"   :> Capture "date"   Day
                                   :> "room"   :> Capture "room"   Room
                                   :> "sensor" :> Capture "sensor" Sensor
                                   :> Get     '[JSON] [SensorReading]
            :<|> "sensor-readings" :> ReqBody '[JSON] SensorReading
                                   :> Put     '[JSON] ()

--------------------------------------------------------------------------------

sensorServer :: ServerT SensorApi AppM
sensorServer = getSensorReadings
          :<|> putSensorReading

getSensorReadings :: Day -> Room -> Sensor -> AppM [SensorReading]
getSensorReadings day room sensor =
  fmap (map entityVal) $ runDb $ selectList [ SensorReadingDate   ==. day
                                            , SensorReadingRoom   ==. room
                                            , SensorReadingSensor ==. sensor
                                            ]
                                            []

putSensorReading :: SensorReading -> AppM ()
putSensorReading sensorReading = do
  _ <- runDb (insert sensorReading)
  return ()
