{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Api.Device
  ( DeviceApi
  , deviceServer
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process  (send, expect)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Reader         (ask)
import Servant
import Sarah.Middleware.Distributed (sendWithPid)
import Sarah.Middleware.Model       (Config (..), MiddlewareApp, unMaster)
import Sarah.Middleware.Types       (Command, Query, QueryResult)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

type DeviceApi = "device" :> "query"
                          :> ReqBody '[JSON] Query
                          :> Get     '[JSON] QueryResult

--------------------------------------------------------------------------------

deviceServer :: ServerT DeviceApi MiddlewareApp
deviceServer = queryServer

queryServer :: Query -> MiddlewareApp QueryResult
queryServer query = do
  Config{..} <- ask
  liftIO . runLocally $ sendWithPid (unMaster master) query >> expect
