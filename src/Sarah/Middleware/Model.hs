{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  ( MiddlewareApp (..), runApp, runEIO
  , Master (..), Slave (..)
  , Config (..)
  , PortManager (..), DeviceController (..)
  , IsDevice (..), mkCommand
  ) where
--------------------------------------------------------------------------------
import Control.Applicative                        (Applicative)
import Control.Distributed.Process                (Process)
import Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import Data.Aeson                                 (ToJSON, FromJSON, encode)
import Data.Text.Encoding                         (decodeUtf8)
import Data.Typeable                              (Typeable)
import GHC.Generics                               (Generic)
import Network.HTTP.Client                        (Manager)
import Sarah.Middleware.Types                     (Command (..), DeviceName, NodeName)
import Servant                                    (ServantErr)
import Servant.Common.BaseUrl                     (BaseUrl)
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy as BS (toStrict)
--------------------------------------------------------------------------------

newtype MiddlewareApp a = MiddlewareApp { unMiddlewareApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runApp :: Config -> (MiddlewareApp a -> ExceptT ServantErr IO a)
runApp config = flip runReaderT config . unMiddlewareApp

runEIO :: (MonadIO m) => ExceptT e IO a -> m (Either e a)
runEIO = liftIO . runExceptT

--------------------------------------------------------------------------------

newtype Master = Master { unMaster :: ProcessId } deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  { unSlave  :: ProcessId } deriving (Eq, Generic, Typeable, Show)

data Config = Config { master     :: Master
                     , localNode  :: LocalNode
                     , runLocally :: forall a. Process a -> IO a
                     , manager    :: Manager
                     , backend    :: BaseUrl
                     }

newtype PortManager      = PortManager ProcessId
newtype DeviceController = DeviceController { unDeviceController :: ProcessId }

-- models can be devices
class (ToJSON model, FromJSON model, ToJSON (DeviceCommand model), FromJSON (DeviceCommand model)) => IsDevice (model :: *) where
  -- the state of a device
  type DeviceState model :: *

  -- the commands a device understands
  data DeviceCommand model :: *

  -- a device controller runs a process for a device, takes commands and executes them
  startDeviceController :: model -> Slave -> PortManager -> Process DeviceController

mkCommand :: IsDevice model => DeviceCommand model -> Command
mkCommand = Command . decodeUtf8 . BS.toStrict . encode
