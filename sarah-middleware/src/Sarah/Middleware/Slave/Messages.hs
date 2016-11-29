{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave.Messages
  where
--------------------------------------------------------------------------------
import Import.MkBinary
--------------------------------------------------------------------------------

data Terminate = Terminate deriving (Binary, Generic, Typeable)
