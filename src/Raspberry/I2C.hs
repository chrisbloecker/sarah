{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Raspberry.I2C
  ( Address (..)
  ) where
--------------------------------------------------------------------------------
import Import.DeriveJSON
--------------------------------------------------------------------------------

newtype Address = Address { unAddress :: Int } deriving (Show, Eq)

deriveJSON jsonOptions ''Address
