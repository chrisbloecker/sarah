{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
--------------------------------------------------------------------------------
module Api
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
import          Messages
import          Network.Wai
import          Network.Wai.Handler.Warp
import          Servant
import          Types
--------------------------------------------------------------------------------
import          Api.Sensor
--------------------------------------------------------------------------------

type Api = SensorApi

--------------------------------------------------------------------------------

runAppProcess :: Process a -> AppM a
runAppProcess p = do
  Config{..} <- ask
  liftIO $ do
    mvar <- newEmptyMVar
    runProcess localNode $ do
      res <- p
      liftIO $ putMVar mvar res
    takeMVar mvar

apiServer :: ServerT Api AppM
apiServer = sensorServer

appToServer :: Config -> Server Api
appToServer config = enter (convertApp config) apiServer

convertApp :: Config -> AppM :~> ExceptT ServantErr IO
convertApp config = Nat (runApp config)

app :: Config -> Application
app config = serve (Proxy :: Proxy Api) (appToServer config)
