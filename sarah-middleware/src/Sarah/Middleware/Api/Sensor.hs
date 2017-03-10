{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api.Sensor
  ( SensorApi
  , sensorServer
  ) where
--------------------------------------------------------------------------------
import Control.Monad.Reader     (ask, lift)
import Control.Monad.Except     (runExceptT, liftIO)
import Data.Time.Calendar       (Day)
import Sarah.Middleware.Model
import Sarah.Persist.Model      (Room, Sensor, SensorReading)
import Servant
import Servant.Client
--------------------------------------------------------------------------------
import qualified Sarah.Persist.Client as Persist
--------------------------------------------------------------------------------

type SensorApi = "sensor-readings" :> "date"   :> Capture "date"   Day
                                   :> "room"   :> Capture "room"   Room
                                   :> "sensor" :> Capture "sensor" Sensor
                                   :> Get '[JSON] [SensorReading]

--------------------------------------------------------------------------------

sensorServer :: ServerT SensorApi MiddlewareApp
sensorServer = getSensorReadings

getSensorReadings :: Day -> Room -> Sensor -> MiddlewareApp [SensorReading]
getSensorReadings day room sensor = do
  Config{..} <- ask
  mreadings <- liftIO $ runClientM (Persist.getSensorReadings day room sensor) (ClientEnv manager backend)
  case mreadings of
    Left err -> return []
    Right readings -> return readings
