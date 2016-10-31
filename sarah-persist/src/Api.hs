{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Api
  ( app
  ) where
--------------------------------------------------------------------------------
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (ExceptT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text            (Text)
import Network.Wai          (Application)
import Servant
import Types
--------------------------------------------------------------------------------
import Api.Sensor
--------------------------------------------------------------------------------

type Api = SensorApi :<|> Raw

--------------------------------------------------------------------------------

sensorApp :: Config -> Application
sensorApp config = serve (Proxy :: Proxy SensorApi) (appToServer config)

appToServer :: Config -> Server SensorApi
appToServer config = enter (convertApp config) sensorServer

convertApp :: Config -> AppM :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runApp)

files :: Application
files = serveDirectory "assets"

app :: Config -> Application
app config = serve (Proxy :: Proxy Api) (appToServer config :<|> files)
