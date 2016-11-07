{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Api.Sensor
  ( SensorApi
  , sensorServer
  ) where
--------------------------------------------------------------------------------
import           Control.Monad.Reader     (ask, lift)
import           Control.Monad.Except     (runExceptT, liftIO)
import           Data.Time.Calendar       (Day)
import           Model
import           Persist.Client
import           Persist.Types
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
  Config{..} <- ask
  mreadings <- liftIO . runExceptT $ getSensorReadingsClient day room sensor manager backend
  case mreadings of
    Left err -> return []
    Right readings -> return readings
