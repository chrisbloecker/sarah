{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
--------------------------------------------------------------------------------
module Api
  ( app
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent.MVar     (newEmptyMVar, takeMVar, putMVar)
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad.Except        (MonadError, ExceptT, mapExceptT)
import Control.Monad.Reader        (MonadIO, MonadReader, ReaderT, runReaderT, runReader, ask, lift)
import Messages
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types
--------------------------------------------------------------------------------

type Api = Get '[JSON] Integer

--newtype AppM a = App { runApp :: ExceptT ServantErr (ReaderT Config Process) a }
--  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

newtype AppM a = App { runApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

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

fortyTwo :: AppM Integer
fortyTwo = do
  Config{..} <- ask
  runAppProcess $ do
    self <- getSelfPid
    send masterPid (EchoMsg self)
  return 42

apiServer :: ServerT Api AppM
apiServer = fortyTwo

appToServer :: Config -> Server Api
appToServer config = enter (convertApp config) apiServer

convertApp :: Config -> AppM :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runApp)
--convertApp :: LocalProcess -> Config -> AppM :~> ExceptT ServantErr IO
--convertApp localProcess config = Nat (mapExceptT (flip runReaderT localProcess . unProcess . flip runReaderT config) . runApp)

app :: Config -> Application
app config = serve (Proxy :: Proxy Api) (appToServer config)
