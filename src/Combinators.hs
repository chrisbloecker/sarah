module Combinators
  where

import Control.Monad ((>=>))

over :: (Monad m, Functor f) => m a -> (a -> f b) -> m (f b)
over ma afb = fmap afb ma

over2 :: (Monad m, Monad n) => m a -> (a -> n b) -> (b -> n c) -> m (n c)
over2 ma anb bnc = over ma (anb >=> bnc)
