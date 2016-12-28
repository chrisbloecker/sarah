module Sarah.Middleware.Client
  where
--------------------------------------------------------------------------------
import           Servant
import           Servant.Client
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Api.Device as Device
import qualified Sarah.Middleware.Api.Status as Status
--------------------------------------------------------------------------------

runAcServer = client (Proxy :: Proxy Device.DeviceApi)
getStats    = client (Proxy :: Proxy Status.StatusApi)
