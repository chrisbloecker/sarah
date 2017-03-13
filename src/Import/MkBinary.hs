module Import.MkBinary
  ( Binary, put, get, putWord8, getWord8
  , Typeable
  , Generic
  ) where

--------------------------------------------------------------------------------
import Data.Binary      (Binary, put, get, putWord8, getWord8)
import Data.Typeable    (Typeable)
import GHC.Generics     (Generic)
--------------------------------------------------------------------------------
