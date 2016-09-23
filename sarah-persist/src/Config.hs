{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
  where
--------------------------------------------------------------------------------
import Control.Monad.Except     (MonadError, ExceptT)
import Control.Monad.Logger     (runStderrLoggingT)
import Control.Monad.Reader     (MonadIO, MonadReader, ReaderT)
import Database.Persist.MySQL   (ConnectInfo (..), createMySQLPool, defaultConnectInfo)
import Database.Persist.Sql     (ConnectionPool)
import Servant                  (ServantErr)
--------------------------------------------------------------------------------
import Settings
--------------------------------------------------------------------------------

newtype AppM a = App { runApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

data Config = Config { getPool :: ConnectionPool }

--------------------------------------------------------------------------------

mkPool :: Settings -> IO ConnectionPool
mkPool settings =
  let connectInfo = defaultConnectInfo { connectHost     = dbHost     settings
                                       , connectPort     = fromIntegral
                                                         . dbPort   $ settings
                                       , connectUser     = dbUser     settings
                                       , connectPassword = dbPassword settings
                                       , connectDatabase = dbDatabase settings
                                       }
  in runStderrLoggingT (createMySQLPool connectInfo 1)
