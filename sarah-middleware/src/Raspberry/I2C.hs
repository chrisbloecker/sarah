module Raspberry.I2C
  ( I2C (..)
  ) where
--------------------------------------------------------------------------------

newtype I2C = I2C { unI2C :: Int } deriving (Show, Eq)
