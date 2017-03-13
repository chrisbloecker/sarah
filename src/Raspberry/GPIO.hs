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
import Data.Binary       (Binary)
import Data.Bits         (Bits)
import Data.ByteString   (ByteString)
import Data.Typeable     (Typeable)
import Foreign.C.Types   (CInt (..))
import GHC.Generics      (Generic)
import Import.DeriveJSON
--------------------------------------------------------------------------------

class ToBits a where
  toBits :: (Num b, Bits b) => a -> b

--------------------------------------------------------------------------------

deriving instance Binary CInt
deriving instance Generic CInt

newtype Pin = Pin { unPin :: CInt } deriving (Binary, Generic, Typeable, Show, Eq)

deriveJSON jsonOptions ''CInt
deriveJSON jsonOptions ''Pin
