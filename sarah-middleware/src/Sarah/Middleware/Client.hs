module Sarah.Middleware.Client
  ( runDeviceCommand
  , getStatus
  ) where
--------------------------------------------------------------------------------
import           Servant
import           Servant.Client
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Api.Device as Device
import qualified Sarah.Middleware.Api.Status as Status
--------------------------------------------------------------------------------

runDeviceCommand = client (Proxy :: Proxy Device.DeviceApi)
getStatus        = client (Proxy :: Proxy Status.StatusApi)
