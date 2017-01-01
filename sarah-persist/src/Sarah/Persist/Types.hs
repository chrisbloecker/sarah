{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
module Sarah.Persist.Types
  where
--------------------------------------------------------------------------------
import Data.Aeson.TH
import Data.Bifunctor       (bimap)
import Data.Text            (pack, unpack)
import Database.Persist.TH
import GHC.Generics         (Generic)
import Servant              (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read            (readEither)
--------------------------------------------------------------------------------

data LogLevel = Info | Debug | Error                     deriving (Show, Read, Eq, Generic)
data Room     = Bedroom | Livingroom | Kitchen | Office  deriving (Show, Read, Eq, Generic)
data Sensor   = Temperature | Humidity | Pressure        deriving (Show, Read, Eq, Generic)

instance FromHttpApiData LogLevel  where parseUrlPiece = bimap pack id . readEither . unpack
instance FromHttpApiData Room      where parseUrlPiece = bimap pack id . readEither . unpack
instance FromHttpApiData Sensor   where parseUrlPiece = bimap pack id . readEither . unpack

instance ToHttpApiData LogLevel  where toUrlPiece = pack . show
instance ToHttpApiData Room      where toUrlPiece = pack . show
instance ToHttpApiData Sensor   where toUrlPiece = pack . show

deriveJSON defaultOptions ''LogLevel
deriveJSON defaultOptions ''Room
deriveJSON defaultOptions ''Sensor

derivePersistField "LogLevel"
derivePersistField "Room"
derivePersistField "Sensor"
