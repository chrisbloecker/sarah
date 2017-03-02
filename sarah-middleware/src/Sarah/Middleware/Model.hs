{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  ( MiddlewareApp (..), runApp, runEIO
  , Master (..), Slave (..)
  , Config (..)
  , PortManager (..), DeviceController (..)
  , IsDevice (..)
  ) where
--------------------------------------------------------------------------------
import Control.Applicative
import Control.Distributed.Process                (Process, expect, say, spawnLocal)
import Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import Control.Lens                               hiding ((.=))
import Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import Data.ByteString.Lazy                       (ByteString)
import Data.Typeable
import Import.DeriveJSON
import Import.MkBinary
import Network.HTTP.Client                        (Manager)
import Sarah.Middleware.Types                     (DeviceName, NodeName)
import Servant                                    (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import Servant.Common.BaseUrl                     (BaseUrl)
import Text.Read                                  (readEither)
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy as BS (fromStrict)
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

newtype PortManager      = PortManager ProcessId
newtype DeviceController = DeviceController { unDeviceController :: ProcessId }

-- models can be devices
class IsDevice model where
  -- the state of a device
  type DeviceState model :: *

  -- the commands a device understands
  data DeviceCommand model :: *

  -- a device controller runs a process for a device, takes commands and executes them
  startDeviceController :: model -> PortManager -> Process DeviceController
