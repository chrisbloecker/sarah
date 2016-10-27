module Persist.Client
  where
--------------------------------------------------------------------------------
import          Api.Sensor
import          Servant
import          Servant.Client
--------------------------------------------------------------------------------

getSensorReadingsClient :<|> putSensorReadingClient = client (Proxy :: Proxy SensorApi)
