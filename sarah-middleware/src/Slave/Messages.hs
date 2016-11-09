{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
--------------------------------------------------------------------------------
module Slave.Messages
  where
--------------------------------------------------------------------------------
import Import.MkBinary
--------------------------------------------------------------------------------

data Terminate = Terminate deriving (Binary, Generic, Typeable)
