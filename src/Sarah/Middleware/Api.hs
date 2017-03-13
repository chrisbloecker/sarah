{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api
  ( app
  ) where
--------------------------------------------------------------------------------
import          Control.Concurrent.MVar                  (newEmptyMVar, takeMVar, putMVar)
import          Control.Distributed.Process
import          Control.Distributed.Process.Internal.Types
import          Control.Distributed.Process.Node
import          Control.Distributed.Process.Serializable (Serializable)
import          Control.Monad.Except                     (ExceptT)
import          Control.Monad.Reader                     (MonadIO, MonadReader, ReaderT, runReaderT, runReader, ask, lift)
import          Network.Wai
import          Network.Wai.Handler.Warp
import          Sarah.Middleware.Model
import          Servant
--------------------------------------------------------------------------------
import          Sarah.Middleware.Api.Device
import          Sarah.Middleware.Api.Sensor
import          Sarah.Middleware.Api.Status
--------------------------------------------------------------------------------

type MiddlewareApi = DeviceApi
                :<|> SensorApi
                :<|> StatusApi

--------------------------------------------------------------------------------

apiServer :: ServerT MiddlewareApi MiddlewareApp
apiServer = deviceServer
       :<|> sensorServer
       :<|> statusServer

appToServer :: Config -> Server MiddlewareApi
appToServer config = enter (convertApp config) apiServer

convertApp :: Config -> MiddlewareApp :~> ExceptT ServantErr IO
convertApp config = Nat (runApp config)

app :: Config -> Application
app config = serve (Proxy :: Proxy MiddlewareApi) (appToServer config)
