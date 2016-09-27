{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Api
  ( app
  ) where
--------------------------------------------------------------------------------
import Control.Monad.Except        (ExceptT)
import Control.Monad.Reader        (runReaderT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text                   (Text)
import Network.Wai                 (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
--------------------------------------------------------------------------------
import Api.Sensor
import Config
--------------------------------------------------------------------------------

type Api = SensorApi :<|> Raw

--------------------------------------------------------------------------------

sensorApp :: Config -> Application
sensorApp config = simpleCors (serve (Proxy :: Proxy SensorApi) (appToServer config))

appToServer :: Config -> Server SensorApi
appToServer config = enter (convertApp config) sensorServer

convertApp :: Config -> AppM :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runApp)

files :: Application
files = serveDirectory "assets"

app :: Config -> Application
app config = serve (Proxy :: Proxy Api) (appToServer config :<|> files)
