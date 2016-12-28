{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
--------------------------------------------------------------------------------
module Raspberry.GPIO
  ( ToBits (..)
  , Pin (..)
  ) where
--------------------------------------------------------------------------------
import Data.Bits         (Bits)
import Data.ByteString   (ByteString)
import Foreign.C.Types   (CInt (..))
import Import.DeriveJSON
import Import.MkBinary
--------------------------------------------------------------------------------

class ToBits a where
  toBits :: (Num b, Bits b) => a -> b

--------------------------------------------------------------------------------

deriving instance Binary CInt
deriving instance Generic CInt

newtype Pin = Pin { unPin :: CInt } deriving (Binary, Generic, Typeable, Show, Eq)

deriveJSON jsonOptions ''CInt
deriveJSON jsonOptions ''Pin
