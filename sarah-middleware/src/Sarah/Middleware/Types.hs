{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Types
  where
--------------------------------------------------------------------------------
import Data.Aeson         (ToJSON, FromJSON, eitherDecode')
import Data.Binary        (Binary)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable      (Typeable)
import GHC.Generics       (Generic)
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS (fromStrict)

type NodeName   = Text
type DeviceName = Text

-- A device can be uniquely identified by a pair of NodeName and DeviceName.
-- This implied that every node must have a unique name. Similarly, every device
-- that is connected to a node has to have a unique name.
-- ToDo: make sure this requirement holds!
data DeviceAddress = DeviceAddress { deviceNode :: NodeName
                                   , deviceName :: DeviceName
                                   }
  deriving (Binary, Generic, Typeable, ToJSON, FromJSON, Eq)


-- A Command is sent to devices in order to tell them what to do.
-- The Text inside the command is a JSON representation of the command, which
-- of course is device-sepcific. If a device receives a command that is intended
-- for a different device type, it should just be ignored.
newtype Command = Command { unCommand :: Text } deriving (Binary, Generic, Typeable, ToJSON, FromJSON)

-- Turn the representation of a command into an actual command for a specific device.
getCommand :: FromJSON a => Command -> Either String a
getCommand = eitherDecode' . BS.fromStrict . encodeUtf8 . unCommand

data Query = Query { queryTarget  :: DeviceAddress
                   , queryCommand :: Command
                   }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON)

data QueryResult = QueryResult
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON)
