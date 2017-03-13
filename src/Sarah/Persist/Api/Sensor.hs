{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Sarah.Persist.Api.Sensor
  ( SensorApi
  , sensorServer
  ) where
--------------------------------------------------------------------------------
import Data.Time.Calendar  (Day)
import Database.Persist
import Sarah.Persist.Model
import Sarah.Persist.Types
import Servant
import Servant.Client
--------------------------------------------------------------------------------

type SensorApi = "sensor-readings" :> "date"   :> Capture "date"   Day
                                   :> "room"   :> Capture "room"   Room
                                   :> "sensor" :> Capture "sensor" Sensor
                                   :> Get     '[JSON] [SensorReading]
            :<|> "sensor-readings" :> ReqBody '[JSON] SensorReading
                                   :> Put     '[JSON] ()

--------------------------------------------------------------------------------

sensorServer :: ServerT SensorApi PersistApp
sensorServer = getSensorReadings
          :<|> putSensorReading

getSensorReadings :: Day -> Room -> Sensor -> PersistApp [SensorReading]
getSensorReadings day room sensor =
  fmap (map entityVal) $ runDb $ selectList [ SensorReadingDate   ==. day
                                            , SensorReadingRoom   ==. room
                                            , SensorReadingSensor ==. sensor
                                            ]
                                            []

putSensorReading :: SensorReading -> PersistApp ()
putSensorReading sensorReading = do
  _ <- runDb (insert sensorReading)
  return ()
