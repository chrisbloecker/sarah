{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Raspberry.GPIO
  ( ToBits (..)
  , Pin (..)
  ) where
--------------------------------------------------------------------------------
import Data.Bits         (Bits)
import Data.ByteString   (ByteString)
import Foreign.C.Types   (CInt)
import Import.DeriveJSON
--------------------------------------------------------------------------------

class ToBits a where
  toBits :: (Num b, Bits b) => a -> b

--------------------------------------------------------------------------------

newtype Pin = Pin { unPin :: CInt } deriving (Show, Eq)

deriveJSON jsonOptions ''CInt
deriveJSON jsonOptions ''Pin
