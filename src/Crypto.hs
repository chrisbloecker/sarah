{-# LANGUAGE GADTs #-}

module Crypto
  ( autokey
  ) where

import Data.Bits (Bits, xor)

autokey :: Bits b => b -> [b] -> [b]
autokey _   []     = []
autokey key (b:bs) = let key' = b `xor` key
                     in key' : autokey key' bs
