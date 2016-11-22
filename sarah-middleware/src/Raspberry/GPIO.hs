module Raspberry.GPIO
  ( ToBits (..)
  , Pin (..)
  , Pulses (..)
  ) where
--------------------------------------------------------------------------------
import Data.Bits       (Bits)
import Data.ByteString (ByteString)
import Data.Vector     (Vector)
import Foreign.C.Types
--------------------------------------------------------------------------------

class ToBits a where
  toBits :: (Num b, Bits b) => a -> b

--------------------------------------------------------------------------------

newtype Pin    = Pin    { unPin    :: CInt       } deriving (Eq)
newtype Pulses = Pulses { unPulses :: Vector Int }
