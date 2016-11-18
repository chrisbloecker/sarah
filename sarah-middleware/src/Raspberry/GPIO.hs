module Raspberry.GPIO
  where
--------------------------------------------------------------------------------
import Foreign.C.Types
--------------------------------------------------------------------------------

newtype Pin = Pin { unPin :: CInt } deriving (Eq)
