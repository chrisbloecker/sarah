{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Types
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process (ProcessId)
import Data.Aeson                  (ToJSON, FromJSON, encode, decode', eitherDecode')
import Data.Binary                 (Binary)
import Data.Text                   (Text)
import Data.Text.Encoding          (encodeUtf8, decodeUtf8)
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS (toStrict, fromStrict)
--------------------------------------------------------------------------------

type NodeName   = Text
type DeviceName = Text

-- A device can be uniquely identified by a pair of NodeName and DeviceName.
-- This implied that every node must have a unique name. Similarly, every device
-- that is connected to a node has to have a unique name.
-- ToDo: make sure this requirement holds!
data DeviceAddress = DeviceAddress { deviceNode :: NodeName
                                   , deviceName :: DeviceName
                                   }
  deriving (Binary, Generic, Typeable, ToJSON, FromJSON, Eq, Show)

-- A wrapper that is intended to be used to add the pid of a sending process
data FromPid message = FromPid ProcessId message deriving (Generic, Binary)

-- Text that is tagged as encoded JSON.
-- ToDo: Probably we should store the type of the original value somewhere using Typeable?
newtype EncodedJSON = EncodedJSON { getEncoded :: Text } deriving (Generic, Binary, ToJSON, FromJSON)

-- A Command is sent to devices in order to tell them what to do.
-- The Text inside the command is a JSON representation of the command, which
-- of course is device-sepcific. If a device receives a command that is intended
-- for a different device type, it should just be ignored.
newtype Command = Command { unCommand :: Text } deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

-- Turn the representation of a command into an actual command for a specific device.
getCommand :: FromJSON a => Command -> Either String a
getCommand = eitherDecode' . BS.fromStrict . encodeUtf8 . unCommand

-- A Query is intended for a specific device and carries a command. This command
-- must be of appropriate "type" in order for the device to be able to understand it.
data Query = Query { queryTarget  :: DeviceAddress
                   , queryCommand :: Command
                   }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

-- ToDo: Is this extra layer of wrapping necessary for CloudHaskell to handle this as a message correctly?
newtype QueryResult = QueryResult { unQueryResult :: Result } deriving (Generic, Binary, Typeable, ToJSON, FromJSON)

-- A Result (carried by a QueryResult) can either be an Error or a Success.
-- The actual result value is a JSON value encoded to a text.
data Result = Error   { unError   :: Text        }
            | Success { unSuccess :: EncodedJSON }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON)

mkError :: Text -> QueryResult
mkError message = QueryResult (Error message)

mkSuccess :: (Typeable a, ToJSON a, FromJSON a) => a -> QueryResult
mkSuccess result = QueryResult (Success $ encodeAndWrap result)

encodeAsText :: (Typeable a, ToJSON a) => a -> Text
encodeAsText = decodeUtf8 . BS.toStrict . encode

decodeFromText :: (Typeable a, FromJSON a) => Text -> Maybe a
decodeFromText = decode' . BS.fromStrict . encodeUtf8

encodeAndWrap :: (Typeable a, ToJSON a, FromJSON a) => a -> EncodedJSON
encodeAndWrap = EncodedJSON . encodeAsText

decodeWrapped :: (Typeable a, ToJSON a, FromJSON a) => EncodedJSON -> Maybe a
decodeWrapped = decodeFromText . getEncoded
