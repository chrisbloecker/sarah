{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module Messages
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process (ProcessId)
import Data.Binary                 (Binary)
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)
--------------------------------------------------------------------------------

data EchoMsg        = EchoMsg        { echoMsgSender :: ProcessId } deriving (Generic, Typeable)
data GetReadingsMsg = GetReadingsMsg { dummy         :: Int       } deriving (Generic, Typeable)
data Terminate      = Terminate                                     deriving (Generic, Typeable)

instance Binary EchoMsg
instance Binary GetReadingsMsg
instance Binary Terminate
