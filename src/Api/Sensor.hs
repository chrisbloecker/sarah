{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Api.Sensors
  where
--------------------------------------------------------------------------------
import           Control.Monad.Reader     (ask, lift)
import           Data.Time.Calendar       (Day)
import           Model
import           Servant
import           Types
--------------------------------------------------------------------------------

type SensorApi = "sensor-readings" :> "date"   :> Capture "date"   Day
                                   :> "room"   :> Capture "room"   Room
                                   :> "sensor" :> Capture "sensor" Sensor
                                   :> Get     '[JSON] [SensorReading]

--------------------------------------------------------------------------------

sensorServer :: ServerT SensorApi AppM
sensorServer = getSensorReadings

getSensorReadings :: Day -> Room -> Sensor -> AppM [SensorReading]
getSensorReadings day room sensor = do
  Config {..} <- ask
  readings <- getSensorReadingsClient day room sensor
  undefined
