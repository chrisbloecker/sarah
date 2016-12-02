{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import           Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import           Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import           Data.Typeable                              (Typeable)
import           GHC.Generics                               (Generic)
import           Import.DeriveJSON
import           Network.HTTP.Client                        (Manager)
import           Raspberry.GPIO                             (Pin)
import           Servant                                    (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import           Servant.Common.BaseUrl                     (BaseUrl)
import           Text.Read                                  (readEither)
--------------------------------------------------------------------------------

newtype MiddlewareApp a = MiddlewareApp { unMiddlewareApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runApp :: Config -> (MiddlewareApp a -> ExceptT ServantErr IO a)
runApp config = flip runReaderT config . unMiddlewareApp

runEIO :: (MonadIO m) => ExceptT e IO a -> m (Either e a)
runEIO = liftIO . runExceptT

--------------------------------------------------------------------------------

data Config = Config { masterPid :: ProcessId
                     , localNode :: LocalNode
                     , manager   :: Manager
                     , backend   :: BaseUrl
                     }

--------------------------------------------------------------------------------

type Host = String
type Port = Int

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  ProcessId deriving (Eq, Generic, Typeable)

data Device = Device { deviceName :: String
                     , deviceType :: DeviceType
                     , interface  :: Interface
                     }

data DeviceType = Toshiba

data Interface = GPIO Pin

deriveJSON jsonOptions ''Device
deriveJSON jsonOptions ''DeviceType
deriveJSON jsonOptions ''Interface
