{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Physics
  where

import Data.Aeson   (ToJSON, FromJSON)
import GHC.Generics (Generic)

newtype Temperature = Temperature { getTemperature :: Double } deriving (Generic, ToJSON, FromJSON, Show, Eq, Ord)
newtype Humidity    = Humidity    { getHumidity    :: Double } deriving (Generic, ToJSON, FromJSON, Show, Eq, Ord)
