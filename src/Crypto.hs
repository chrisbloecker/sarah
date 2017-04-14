module Crypto
  ( autokey, unautokey
  ) where

import Data.Bits (Bits, xor)

autokey :: Bits b => b -> [b] -> [b]
autokey k bs = zipWith xor (scanl xor k bs) bs

unautokey :: Bits b => b -> [b] -> [b]
unautokey k bs = zipWith xor (k:bs) bs
