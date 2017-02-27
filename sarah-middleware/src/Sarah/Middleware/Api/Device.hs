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
import           Sarah.Middleware.Device
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

type DeviceApi = "device" :> "set"
                          :> ReqBody '[JSON] Command
                          :> Post    '[JSON] Command
            :<|> "device" :> "get"
                          :> ReqBody '[JSON] Query
                          :> Get     '[JSON] QueryResult

--------------------------------------------------------------------------------

deviceServer :: ServerT DeviceApi MiddlewareApp
deviceServer = setServer :<|> getServer

setServer :: Command -> MiddlewareApp Command
setServer config = undefined --liftIO $ Toshiba.send (Pin 23) config >> return config

getServer :: Query -> MiddlewareApp eQueryResult
getServer = undefined
