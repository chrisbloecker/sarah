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
import Sarah.Persist.Api.Log
import Sarah.Persist.Api.Sensor
import Sarah.Persist.Types
import Servant
--------------------------------------------------------------------------------

type PersistApi = LogApi
             :<|> SensorApi

--------------------------------------------------------------------------------

apiServer :: ServerT PersistApi PersistApp
apiServer = logServer
       :<|> sensorServer

appToServer :: Config -> Server PersistApi
appToServer config = enter (convertApp config) apiServer

convertApp :: Config -> PersistApp :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runPersistApp)

app :: Config -> Application
app config = serve (Proxy :: Proxy PersistApi) (appToServer config)
