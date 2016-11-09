{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
module Types
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import           Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import           Control.Monad.Except                       (MonadError, ExceptT)
import           Data.Bifunctor                             (bimap)
import           Data.Text                                  (pack, unpack)
import           Data.Typeable                              (Typeable)
import           GHC.Generics                               (Generic)
import           Import.DeriveJSON
import           Network.HTTP.Client                        (Manager)
import           Servant                                    (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import           Servant.Common.BaseUrl                     (BaseUrl)
import           Text.Read                                  (readEither)
--------------------------------------------------------------------------------

newtype AppM a = App { unApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runApp :: Config -> (AppM a -> ExceptT ServantErr IO a)
runApp config = flip runReaderT config . unApp

--------------------------------------------------------------------------------

data Config = Config { masterPid :: ProcessId
                     , localNode :: LocalNode
                     , backend   :: BaseUrl
                     , manager   :: Manager
                     }

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  ProcessId deriving (Eq, Generic, Typeable)
