{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  ( module Sarah.Middleware.Model
  ) where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Distributed.Process                (Process, expect, say, spawnLocal)
import           Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import           Control.Lens                               hiding ((.=))
import           Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import           Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import           Data.Typeable
import           Import.DeriveJSON
import           Import.MkBinary
import           Network.HTTP.Client                        (Manager)
import           Sarah.Middleware.Device
import           Servant                                    (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import           Servant.Common.BaseUrl                     (BaseUrl)
import           Text.Read                                  (readEither)
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM
--------------------------------------------------------------------------------

newtype MiddlewareApp a = MiddlewareApp { unMiddlewareApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runApp :: Config -> (MiddlewareApp a -> ExceptT ServantErr IO a)
runApp config = flip runReaderT config . unMiddlewareApp

runEIO :: (MonadIO m) => ExceptT e IO a -> m (Either e a)
runEIO = liftIO . runExceptT

--------------------------------------------------------------------------------

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  ProcessId deriving (Eq, Generic, Typeable, Show)

data Config = Config { master     :: Master
                     , localNode  :: LocalNode
                     , runLocally :: forall a. Process a -> IO a
                     , manager    :: Manager
                     , backend    :: BaseUrl
                     }

data NodeInfo = NodeInfo { _nodeName    :: Text
                         , _nodeDevices :: [(Text, DeviceRep)]
                         }
  deriving (Generic, Typeable, Show)
instance Binary NodeInfo
deriveJSON jsonOptions ''NodeInfo
makeLenses ''NodeInfo

data Status = Status { _connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Typeable, Show)
instance Binary Status
deriveJSON jsonOptions ''Status
makeLenses ''Status
