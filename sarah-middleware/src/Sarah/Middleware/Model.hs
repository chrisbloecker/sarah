{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process                (Process)
import           Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import           Control.Lens
import           Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import           Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import           Import.DeriveJSON
import           Import.MkBinary
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

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  ProcessId deriving (Eq, Generic, Typeable, Show)

data Config = Config { master     :: Master
                     , localNode  :: LocalNode
                     , runLocally :: forall a. Process a -> IO a
                     , manager    :: Manager
                     , backend    :: BaseUrl
                     }

--------------------------------------------------------------------------------

type Host = String
type Port = Int

data WebAddress = WebAddress { host :: Host, port :: Port } deriving (Show)
deriveJSON jsonOptions ''WebAddress

--------------------------------------------------------------------------------

type NodeName = Text

{-
data Device = Device { _deviceName      :: Text
                     , _deviceInterface :: Interface a => a
                     }
  deriving (Generic, Typeable, Show)

data DeviceType = AC
                | Sensor
                | TV
-}
-- ToDo: this should be somewhere else
data DeviceModel = Model_Toshiba_16NKV_E
                 | Model_Toshiba_RAS_M13NKCV
                 | Model_Toshiba_RAS_M16NKCV
                 | Model_DHT22

class IsInterface interface where
  startInterfaceController :: interface -> Process ProcessId

data Interface = forall i. IsInterface i => Interface i

instance IsInterface Interface where
  startInterfaceController (Interface interface) = startInterfaceController interface

newtype GPIO = GPIO Pin
instance IsInterface GPIO where
  startInterfaceController = error "startController not implemented for instance IsInterface GPIO"
deriveJSON jsonOptions ''GPIO

newtype I2C = I2C Integer
instance IsInterface I2C where
  startInterfaceController = error "startController not implemented for instance IsInterface I2C"
deriveJSON jsonOptions ''I2C

newtype IP = IP WebAddress
instance IsInterface IP where
  startInterfaceController = error "startController not implemented for instance IsInterface IP"
deriveJSON jsonOptions ''IP

--instance Binary Device
--instance Binary Interface

--makeLenses ''Device

--deriveJSON jsonOptions ''InterfaceDescription
--deriveJSON jsonOptions ''Interface
--deriveJSON jsonOptions ''Device

--------------------------------------------------------------------------------

data Status = Status { _connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Typeable, Show)

data NodeInfo = NodeInfo { _nodeName    :: NodeName
                         , _nodeDevices :: [Text]
                         }
  deriving (Generic, Typeable, Show)

instance Binary Status
instance Binary NodeInfo

makeLenses ''Status
makeLenses ''NodeInfo

deriveJSON jsonOptions ''NodeInfo
deriveJSON jsonOptions ''Status
