module Sarah.Middleware.Client
  where
--------------------------------------------------------------------------------
import           Servant
import           Servant.Client
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Api.Device as Device
--------------------------------------------------------------------------------

runAcServer = client (Proxy :: Proxy Device.DeviceApi)
