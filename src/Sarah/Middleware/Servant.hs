{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Servant
  ( MiddlewareApp
  , runApp
  ) where
--------------------------------------------------------------------------------
import Control.Applicative    (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (ReaderT, MonadReader, runReaderT)
import Control.Monad.Except   (ExceptT, MonadError)
import Data.Functor           (Functor)
import Sarah.Middleware.Model (Config)
import Servant                (ServantErr)
--------------------------------------------------------------------------------

newtype MiddlewareApp a = MiddlewareApp { unMiddlewareApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runApp :: Config -> (MiddlewareApp a -> ExceptT ServantErr IO a)
runApp config = flip runReaderT config . unMiddlewareApp
