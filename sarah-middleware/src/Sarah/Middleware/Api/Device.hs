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
import           Sarah.Middleware.Model
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

type DeviceApi = "device" :> "set"
                          :> ReqBody '[JSON] DeviceCommand
                          :> Post    '[JSON] DeviceCommand
            :<|> "device" :> "get"
                          :> ReqBody '[JSON] DeviceQuery
                          :> Get     '[JSON] DeviceQueryResult

--------------------------------------------------------------------------------

deviceServer :: ServerT DeviceApi MiddlewareApp
deviceServer = setServer :<|> getServer

setServer :: DeviceCommand -> MiddlewareApp DeviceCommand
setServer config = undefined --liftIO $ Toshiba.send (Pin 23) config >> return config

getServer :: DeviceQuery -> MiddlewareApp DeviceQueryResult
getServer = undefined
