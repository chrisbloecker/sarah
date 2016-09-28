{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Network.Wai                 (Application, Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant
--------------------------------------------------------------------------------
import Api.Sensor
import Config
--------------------------------------------------------------------------------

type Api = SensorApi :<|> Raw

--------------------------------------------------------------------------------

sensorApp :: Config -> Application
sensorApp config = myCors (serve (Proxy :: Proxy SensorApi) (appToServer config))

appToServer :: Config -> Server SensorApi
appToServer config = enter (convertApp config) sensorServer

convertApp :: Config -> AppM :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runApp)

files :: Application
files = serveDirectory "assets"

app :: Config -> Application
app config = serve (Proxy :: Proxy Api) (appToServer config :<|> files)

myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
