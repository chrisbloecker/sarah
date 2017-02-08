{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  where
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Distributed.Process                (Process)
import           Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import           Control.Lens                               hiding ((.=))
import           Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import           Control.Monad.Except                       (MonadError, ExceptT, runExceptT, liftIO)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe                                 (isJust)
import           Import.DeriveJSON
import           Import.MkBinary
import           Network.HTTP.Client                        (Manager)
import           Raspberry.GPIO                             (Pin)
import           Raspberry.I2C                              (Address)
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

--------------------------------------------------------------------------------

type Host = String
type Port = Int

data WebAddress = WebAddress { host :: Host, port :: Port } deriving (Show)
deriveJSON jsonOptions ''WebAddress

--------------------------------------------------------------------------------
{-
data Interface = GPIO Pin
               | I2C Address
               | IP WebAddress
  deriving (Eq, Show)
deriveJSON jsonOptions ''Interface
-}
type NodeName = Text

class IsInterface interface where
  startInterfaceController :: interface -> Process ProcessId

newtype GPIO = GPIO Pin deriving (Show)
instance IsInterface GPIO where
  startInterfaceController = error "startController not implemented for instance IsInterface GPIO"
deriveJSON jsonOptions ''GPIO

newtype I2C = I2C Address deriving (Show)
instance IsInterface I2C where
  startInterfaceController = error "startController not implemented for instance IsInterface I2C"
deriveJSON jsonOptions ''I2C

newtype IP = IP WebAddress deriving (Show)
instance IsInterface IP where
  startInterfaceController = error "startController not implemented for instance IsInterface IP"
deriveJSON jsonOptions ''IP

data Interface = forall interface. (IsInterface interface, Show interface, FromJSON interface, ToJSON interface) => Interface interface

instance Show Interface where
  show (Interface interface) = "Interface " ++ show interface
instance ToJSON Interface where
  toJSON (Interface t) = object [ "interface" .= toJSON t ]
instance FromJSON Interface where
  parseJSON = withObject "Interface" $ \o -> do
    interface <- o .: "interface" :: Parser Object

    case HM.lookup "gpio" interface of
      Just _ -> Interface <$> (parseJSON (Object interface) :: Parser GPIO)
      Nothing -> case HM.lookup "i2c" interface of
        Just _ -> Interface <$> (parseJSON (Object interface) :: Parser I2C)
        Nothing -> case HM.lookup "ip" interface of
          Just _ -> Interface <$> (parseJSON (Object interface) :: Parser IP)
          Nothing -> fail "Can't parse Interface"


instance IsInterface Interface where
  startInterfaceController (Interface interface) = startInterfaceController interface

--------------------------------------------------------------------------------

data Status = Status { _connectedNodes :: [NodeInfo]
                     }
  deriving (Generic, Typeable, Show)

data NodeInfo = NodeInfo { _nodeName    :: NodeName
-- ToDo: add this back in
--                         , _nodeDevices :: [Device]
                         }
  deriving (Generic, Typeable, Show)

instance Binary Status
instance Binary NodeInfo

makeLenses ''Status
makeLenses ''NodeInfo

deriveJSON jsonOptions ''NodeInfo
deriveJSON jsonOptions ''Status
