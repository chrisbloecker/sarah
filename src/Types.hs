{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
module Types
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import           Control.Monad.Reader                       (MonadIO, MonadReader, ReaderT, runReaderT, runReader)
import           Control.Monad.Except                       (MonadError, ExceptT)
import           Data.Bifunctor                             (bimap)
import           Data.Text                                  (pack, unpack)
import           GHC.Generics                               (Generic)
import           Import.DeriveJSON
import           Servant                                    (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import           Text.Read                                  (readEither)
--------------------------------------------------------------------------------

newtype AppM a = App { unApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

runApp :: Config -> (AppM a -> ExceptT ServantErr IO a)
runApp config = flip runReaderT config . unApp

--------------------------------------------------------------------------------

data Config = Config { masterPid :: ProcessId
                     , localNode :: LocalNode
                     }

--------------------------------------------------------------------------------

data Room   = Bedroom | Livingroom | Kitchen | Office  deriving (Show, Read, Eq, Generic)
data Sensor = Temperature | Humidity | Pressure        deriving (Show, Read, Eq, Generic)

--------------------------------------------------------------------------------

instance FromHttpApiData Room    where parseUrlPiece = bimap pack id . readEither . unpack
instance FromHttpApiData Sensor  where parseUrlPiece = bimap pack id . readEither . unpack

instance ToHttpApiData Room    where toUrlPiece = pack . show
instance ToHttpApiData Sensor  where toUrlPiece = pack . show

deriveJSON jsonOptions ''Room
deriveJSON jsonOptions ''Sensor
