{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api.Device
  ( DeviceApi
  , deviceServer
  ) where
--------------------------------------------------------------------------------
import           Control.Monad.IO.Class                        (liftIO)
import           Raspberry.GPIO
import           Servant
import           Sarah.Middleware.Types             hiding     (Config)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

type DeviceApi = "device" :> "ac"
                          :> ReqBody '[JSON] Toshiba.Config
                          :> Post    '[JSON] Toshiba.Config

--------------------------------------------------------------------------------

deviceServer :: ServerT DeviceApi MiddlewareApp
deviceServer = acServer

acServer :: Toshiba.Config -> MiddlewareApp Toshiba.Config
acServer config = liftIO $ Toshiba.send (Pin 23) config >> return config
