module Physics
  where

newtype Temperature = Temperature { unTemperature :: Double } deriving (Show)
newtype Humidity    = Humidity    { unHumidity    :: Double } deriving (Show)
