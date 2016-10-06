{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
--------------------------------------------------------------------------------
module Api
  ( app
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process.Internal.Types (LocalProcess)
import Control.Monad.Except        (MonadError, ExceptT)
import Control.Monad.Reader        (MonadIO, MonadReader, ReaderT, runReaderT)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
--------------------------------------------------------------------------------

type Api = Raw

newtype AppM a = App { runApp :: ReaderT LocalProcess (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader LocalProcess, MonadError ServantErr, MonadIO)

--------------------------------------------------------------------------------

convertApp :: LocalProcess -> AppM :~> ExceptT ServantErr IO
convertApp config = Nat (flip runReaderT config . runApp)

app :: Application
app = serve (Proxy :: Proxy Api) (serveDirectory ".")
