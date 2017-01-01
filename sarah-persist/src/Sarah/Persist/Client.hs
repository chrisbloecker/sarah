module Sarah.Persist.Client
  where
--------------------------------------------------------------------------------
import          Sarah.Persist.Api.Log
import          Sarah.Persist.Api.Sensor
import          Servant
import          Servant.Client
--------------------------------------------------------------------------------

getLog :<|> putLog = client (Proxy :: Proxy LogApi)
getSensorReadings :<|> putSensorReading = client (Proxy :: Proxy SensorApi)
