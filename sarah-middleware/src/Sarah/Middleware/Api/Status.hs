{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api.Status
  ( StatusApi
  , statusServer
  ) where
--------------------------------------------------------------------------------
import           Control.Distributed.Process      (Process, getSelfPid, liftIO, expect)
import           Control.Distributed.Process.Node (runProcess)
import           Control.Monad.Reader             (ask)
import           Control.Monad
import           Sarah.Middleware.Model
import           Servant
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Master.Messages as Master
--------------------------------------------------------------------------------

type StatusApi = "status" :> Get '[JSON] Status

statusServer :: ServerT StatusApi MiddlewareApp
statusServer = getStatus

getStatus :: MiddlewareApp Status
getStatus = do
  Config{..} <- ask
  return =<< liftIO . localProcess $ do Master.getStatus master =<< getSelfPid
                                        expect :: Process Status
