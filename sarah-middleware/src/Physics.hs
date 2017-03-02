{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Physics
  where

import Data.Aeson   (ToJSON, FromJSON)
import GHC.Generics (Generic)

newtype Temperature = Temperature { unTemperature :: Double } deriving (Generic, ToJSON, FromJSON, Show)
newtype Humidity    = Humidity    { unHumidity    :: Double } deriving (Generic, ToJSON, FromJSON, Show)
