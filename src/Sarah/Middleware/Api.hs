{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api
  ( app
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process              (send, expect, getSelfPid)
import Control.Distributed.Process.Node
import Control.Monad.IO.Class                   (liftIO)
import Control.Monad.Except                     (ExceptT)
import Control.Monad.Reader                     (ask)
import Network.Wai                       hiding (Request)
import Network.Wai.Handler.Warp
import Sarah.Middleware.Distributed             (Status)
import Sarah.Middleware.Model
import Sarah.Middleware.Servant
import Servant
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Master.Messages as Master (getStatus)
--------------------------------------------------------------------------------

type MiddlewareApi = DeviceApi
                :<|> StatusApi

--------------------------------------------------------------------------------

apiServer :: ServerT MiddlewareApi MiddlewareApp
apiServer = deviceServer
       :<|> statusServer

appToServer :: Config -> Server MiddlewareApi
appToServer config = enter (convertApp config) apiServer

convertApp :: Config -> MiddlewareApp :~> ExceptT ServantErr IO
convertApp config = Nat (runApp config)

app :: Config -> Application
app config = serve (Proxy :: Proxy MiddlewareApi) (appToServer config)

--------------------------------------------------------------------------------
-- the device api

type DeviceApi = "device" :> "query"
                          :> ReqBody '[JSON] Query
                          :> Get     '[JSON] QueryResult

deviceServer :: ServerT DeviceApi MiddlewareApp
deviceServer = queryServer

queryServer :: Query -> MiddlewareApp QueryResult
queryServer query = do
  Config{..} <- ask
  liftIO . runLocally $ do
    sendWithPid (unMaster master) query
    expect

--------------------------------------------------------------------------------
-- the status api

type StatusApi = "status" :> Get '[JSON] Status

statusServer :: ServerT StatusApi MiddlewareApp
statusServer = getStatus

getStatus :: MiddlewareApp Status
getStatus = do
  Config{..} <- ask
  liftIO . runLocally $ do
    Master.getStatus master =<< getSelfPid
    expect
