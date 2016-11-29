module Sarah.Persist.Client
  where
--------------------------------------------------------------------------------
import          Sarah.Persist.Api.Sensor
import          Servant
import          Servant.Client
--------------------------------------------------------------------------------

getSensorReadingsClient :<|> putSensorReadingClient = client (Proxy :: Proxy SensorApi)
