{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api.Device
  ( DeviceApi
  , deviceServer
  ) where
--------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Servant
import           Sarah.Middleware.Model (MiddlewareApp)
import           Sarah.Middleware.Types (Command, Query, QueryResult)
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

getServer :: Query -> MiddlewareApp QueryResult
getServer = undefined
