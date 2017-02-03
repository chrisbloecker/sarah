{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
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

data Device = Device { _deviceName      :: Text
                     , _deviceModel     :: DeviceModel
                     , _deviceInterface :: Interface
                     }
  deriving (Generic, Typeable, Show)

data DeviceModel = AC     ACModel
                 | Sensor SensorModel
                 | TV     TVModel
  deriving (Generic, Typeable, Show, Eq)

data ACModel = Toshiba_RAS_M13NKCV
             | Toshiba_RAS_M16NKCV
             | Toshiba_16NKV_E
  deriving (Generic, Typeable, Show, Eq)

data SensorModel = BPM180
                 | DHT22
  deriving (Generic, Typeable, Show, Eq)

data TVModel = LG
  deriving (Generic, Typeable, Show, Eq)

data Interface = GPIO Pin
  deriving (Generic, Typeable, Show, Eq)

instance Binary Device
instance Binary DeviceModel
instance Binary ACModel
instance Binary SensorModel
instance Binary TVModel
instance Binary Interface

makeLenses ''Device

deriveJSON jsonOptions ''Interface
deriveJSON jsonOptions ''TVModel
deriveJSON jsonOptions ''SensorModel
deriveJSON jsonOptions ''ACModel
deriveJSON jsonOptions ''DeviceModel
deriveJSON jsonOptions ''Device

--------------------------------------------------------------------------------

data Status = Status { _connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Typeable, Show)

data NodeInfo = NodeInfo { _nodeName    :: NodeName
                         , _nodeDevices :: [Device]
                         }
  deriving (Generic, Typeable, Show)

instance Binary Status
instance Binary NodeInfo

makeLenses ''Status
makeLenses ''NodeInfo

deriveJSON jsonOptions ''NodeInfo
deriveJSON jsonOptions ''Status
