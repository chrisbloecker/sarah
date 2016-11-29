{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
--------------------------------------------------------------------------------
module Sarah.Persist.Api
  ( app
  ) where
--------------------------------------------------------------------------------
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (ExceptT)
import Data.Aeson
import Data.Aeson.TH
import Data.Text            (Text)
import Network.Wai          (Application)
import Sarah.Persist.Types
import Servant
--------------------------------------------------------------------------------
import Sarah.Persist.Api.Sensor
--------------------------------------------------------------------------------

type PersistApi = SensorApi

--------------------------------------------------------------------------------

sensorApp :: Config -> Application
sensorApp config = serve (Proxy :: Proxy SensorApi) (appToServer config)

appToServer :: Config -> Server SensorApi
appToServer config = enter (convertApp config) sensorServer

convertApp :: Config -> PersistApp :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runPersistApp)

files :: Application
files = serveDirectory "assets"

app :: Config -> Application
app config = serve (Proxy :: Proxy PersistApi) (appToServer config)
