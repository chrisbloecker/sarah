{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model
  ( Config (..)

  , Master (unMaster)
  , Slave (unSlave)
  , mkMaster
  , mkSlave

  , IsDevice (..)

  , NodeName
  , DeviceName
  , DeviceAddress (..)

  , FromPid (..)
  , sendWithPid

  , Query (queryTarget, queryCommand)
  , mkQuery

  , QueryResult
  , mkQueryResult
  , getQueryResult

  , Command
  , mkCommand
  , getCommand

  , PortManager (..)
  , DeviceController (..)

  , EncodedDeviceState
  , encodeDeviceState
  , decodeDeviceState
  , eitherDecodeDeviceState

  , MiddlewareEvent (..)
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process                (Process, getSelfPid, send)
import Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
import Data.Aeson                                 (ToJSON, FromJSON, encode, eitherDecode', decode')
import Data.Binary                                (Binary)
import Data.ByteString.Lazy                       (ByteString)
import Data.Hashable                              (Hashable)
import Data.Maybe                                 (fromJust)
import Data.Text                                  (Text)
import Data.Text.Encoding                         (encodeUtf8, decodeUtf8)
import Data.Typeable                              (Typeable)
import GHC.Generics                               (Generic)
import Network.HTTP.Client                        (Manager)
import Network.WebSockets                         (WebSocketsData (..))
import Servant.Common.BaseUrl                     (BaseUrl)
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy as LBS (toStrict, fromStrict)
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

data Stream = Stream { streamName :: Text
                     , streamUnit :: Unit
                     , readStream :: 
                     }

-- Devices have a state and a set of commands that can be sent to them.
-- ToDo: do we still need all those ToJSON and FromJSON instances?
-- ToDo: do we still need DeviceCommand in here or is it enough to use the RequestReplyPairs?
class ( ToJSON model, FromJSON model
      , ToJSON (DeviceState model), FromJSON (DeviceState model)
      , ToJSON (DeviceRequest model), FromJSON (DeviceRequest model)
      , ToJSON (DeviceReply   model), FromJSON (DeviceReply   model)
      ) => IsDevice (model :: *) where
  -- the state of a device
  data family DeviceState model :: *

  -- the commands a device understands
  data family DeviceRequest model :: *

  data family DeviceReply   model :: *

  -- a device controller runs a process for a device, takes commands and executes them
  startDeviceController :: model -> Slave -> PortManager -> Process DeviceController

  getStreams :: model -> [Stream]

-- Device states can be serialised and sent over the network. However, without
-- knowledge of the concrete device model at hand, the state can not be interpretet.
newtype EncodedDeviceState = EncodedDeviceState { getState :: Text } deriving (Generic, Binary, ToJSON, FromJSON)

encodeDeviceState :: IsDevice model => DeviceState model -> EncodedDeviceState
encodeDeviceState = EncodedDeviceState . decodeUtf8 . LBS.toStrict . encode

decodeDeviceState :: IsDevice model => EncodedDeviceState -> Maybe (DeviceState model)
decodeDeviceState = decode' . LBS.fromStrict . encodeUtf8 . getState

eitherDecodeDeviceState :: IsDevice model => EncodedDeviceState -> Either String (DeviceState model)
eitherDecodeDeviceState = eitherDecode' . LBS.fromStrict . encodeUtf8 . getState

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

mkCommand :: IsDevice model => DeviceRequest model -> Command
mkCommand = Command . decodeUtf8 . LBS.toStrict . encode

-- Turn the representation of a command into an actual command for a specific device.
getCommand :: IsDevice model => Command -> Either String (DeviceRequest model)
getCommand = eitherDecode' . LBS.fromStrict . encodeUtf8 . unCommand


-- A Query is intended for a specific device and carries a command. This command
-- must be of appropriate "type" in order for the device to be able to understand it.
data Query = Query { queryTarget  :: DeviceAddress
                   , queryCommand :: Command
                   }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

instance WebSocketsData Query where
  toLazyByteString = encode
  fromLazyByteString = fromJust. decode'

mkQuery :: IsDevice model => DeviceAddress -> DeviceRequest model -> Query
mkQuery deviceAddress command = Query deviceAddress (mkCommand command)


newtype QueryResult = QueryResult { unQueryResult :: Text } deriving (Generic, Binary, Typeable, ToJSON, FromJSON)

instance WebSocketsData QueryResult where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

mkQueryResult :: IsDevice model => DeviceReply model -> QueryResult
mkQueryResult = QueryResult . decodeUtf8 . LBS.toStrict . encode

getQueryResult :: IsDevice model => QueryResult -> Either String (DeviceReply model)
getQueryResult = eitherDecode' . LBS.fromStrict . encodeUtf8 . unQueryResult


--------------------------------------------------------------------------------

data MiddlewareEvent = StateChangeEvent DeviceAddress EncodedDeviceState
  deriving (Generic, ToJSON, FromJSON)

instance WebSocketsData MiddlewareEvent where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'
