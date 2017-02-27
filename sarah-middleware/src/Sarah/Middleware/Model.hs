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
import Control.Applicative
import Control.Distributed.Process                (Process, expect, say, spawnLocal)
import Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import Control.Lens                               hiding ((.=))
import Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import Data.Typeable
import Import.DeriveJSON
import Import.MkBinary
import Network.HTTP.Client                        (Manager)
import Servant                                    (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import Servant.Common.BaseUrl                     (BaseUrl)
import Text.Read                                  (readEither)
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

type DeviceName = Text

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  ProcessId deriving (Eq, Generic, Typeable, Show)

data Config = Config { master     :: Master
                     , localNode  :: LocalNode
                     , runLocally :: forall a. Process a -> IO a
                     , manager    :: Manager
                     , backend    :: BaseUrl
                     }

data Command     = Command { commandTarget  :: (Text, DeviceName)
                           , commandCommand :: Text
                           }
data Query       = Query
data QueryResult = QueryResult

deriveJSON jsonOptions ''Command
deriveJSON jsonOptions ''Query
deriveJSON jsonOptions ''QueryResult

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
