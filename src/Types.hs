{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
module Types
  where
--------------------------------------------------------------------------------
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Aeson.TH
import Data.Bifunctor       (bimap)
import Data.Text            (pack, unpack)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.TH
import GHC.Generics         (Generic)
import Servant              (FromHttpApiData (..), ToHttpApiData (..), ServantErr)
import Text.Read            (readEither)
--------------------------------------------------------------------------------

newtype AppM a = App { runApp :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

data Config = Config { getPool :: ConnectionPool }

--------------------------------------------------------------------------------

data Room   = Bedroom | Livingroom | Kitchen | Office  deriving (Show, Read, Eq, Generic)
data Sensor = Temperature | Humidity | Pressure        deriving (Show, Read, Eq, Generic)

--------------------------------------------------------------------------------

instance FromHttpApiData Room    where parseUrlPiece = bimap pack id . readEither . unpack
instance FromHttpApiData Sensor  where parseUrlPiece = bimap pack id . readEither . unpack

instance ToHttpApiData Room    where toUrlPiece = pack . show
instance ToHttpApiData Sensor  where toUrlPiece = pack . show

deriveJSON defaultOptions ''Room
deriveJSON defaultOptions ''Sensor

derivePersistField "Room"
derivePersistField "Sensor"
