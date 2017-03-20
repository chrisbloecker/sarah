{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  ( Config (..)

  , Master (unMaster)
  , Slave (unSlave)
  , mkMaster
  , mkSlave

  , NodeName
  , DeviceName
  , DeviceAddress (..)

  , FromPid (..)
  , sendWithPid

  , Query (..)
  , QueryResult (..)

  , Command
  , mkCommand
  , getCommand

  , Result (..)
  , mkSuccess
  , mkError

  , PortManager (..)
  , DeviceController (..)
  , IsDevice (..)

  , encodeAsText
  , decodeFromText
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process                (Process, getSelfPid, send)
import Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import Data.Aeson                                 (ToJSON, FromJSON, encode, eitherDecode', decode')
import Data.Binary                                (Binary)
import Data.ByteString.Lazy                       (ByteString)
import Data.Hashable                              (Hashable)
import Data.Text                                  (Text)
import Data.Text.Encoding                         (encodeUtf8, decodeUtf8)
import Data.Typeable                              (Typeable)
import GHC.Generics                               (Generic)
import Network.HTTP.Client                        (Manager)
import Servant.Common.BaseUrl                     (BaseUrl)
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy as BS (toStrict, fromStrict)
--------------------------------------------------------------------------------

data Config = Config { master     :: Master
                     , localNode  :: LocalNode
                     , runLocally :: forall a. Process a -> IO a
                     , manager    :: Manager
                     , database   :: BaseUrl
                     }

--------------------------------------------------------------------------------

newtype Master = Master { unMaster :: ProcessId } deriving (Eq, Generic, Typeable, Show)
newtype Slave  = Slave  { unSlave  :: ProcessId } deriving (Eq, Generic, Typeable, Show)

mkMaster :: ProcessId -> Master
mkMaster = Master

mkSlave :: ProcessId -> Slave
mkSlave = Slave

newtype PortManager      = PortManager ProcessId
newtype DeviceController = DeviceController { unDeviceController :: ProcessId }

--------------------------------------------------------------------------------

class ( ToJSON model, FromJSON model
      , ToJSON (DeviceState model), FromJSON (DeviceState model)
      , ToJSON (DeviceCommand model), FromJSON (DeviceCommand model)
      ) => IsDevice (model :: *) where
  -- the state of a device
  data DeviceState model :: *

  -- the commands a device understands
  data DeviceCommand model :: *

  -- a device controller runs a process for a device, takes commands and executes them
  startDeviceController :: model -> Slave -> PortManager -> Process DeviceController

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
  deriving (Binary, Generic, Typeable, ToJSON, FromJSON, Hashable, Eq, Show)


-- A wrapper that is intended to be used to add the pid of a sending process
data FromPid message = FromPid ProcessId message deriving (Generic, Binary)

-- like send, but wraps the message with the pid of the sending process
sendWithPid :: (Binary message, Typeable message) => ProcessId -> message -> Process ()
sendWithPid to message = getSelfPid >>= \self -> send to (FromPid self message)


-- A Command is sent to devices in order to tell them what to do.
-- The Text inside the command is a JSON representation of the command, which
-- of course is device-sepcific. If a device receives a command that is intended
-- for a different device type, it should just be ignored.
newtype Command = Command { unCommand :: Text } deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

mkCommand :: IsDevice model => DeviceCommand model -> Command
mkCommand = Command . decodeUtf8 . BS.toStrict . encode

-- Turn the representation of a command into an actual command for a specific device.
getCommand :: IsDevice model => Command -> Either String (DeviceCommand model)
getCommand = eitherDecode' . BS.fromStrict . encodeUtf8 . unCommand


-- A Query is intended for a specific device and carries a command. This command
-- must be of appropriate "type" in order for the device to be able to understand it.
data Query = Query { queryTarget  :: DeviceAddress
                   , queryCommand :: Command
                   }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)


-- ToDo: Is this extra layer of wrapping necessary for CloudHaskell to handle this as a message correctly?
newtype QueryResult = QueryResult { unQueryResult :: Result } deriving (Generic, Binary, Typeable)


-- A Result (carried by a QueryResult) can either be an Error or a Success.
-- The actual result value is a JSON value encoded to a text.
data Result = Error   { unError   :: Text       }
            | Success { unSuccess :: ByteString }
  deriving (Generic, Binary, Typeable, Show)


mkError :: Text -> QueryResult
mkError message = QueryResult (Error message)

mkSuccess :: ByteString -> QueryResult
mkSuccess result = QueryResult (Success result)

encodeAsText :: (ToJSON a, FromJSON a) => a -> Text
encodeAsText = decodeUtf8 . BS.toStrict . encode

decodeFromText :: (ToJSON a, FromJSON a) => Text -> Maybe a
decodeFromText = decode' . BS.fromStrict . encodeUtf8
