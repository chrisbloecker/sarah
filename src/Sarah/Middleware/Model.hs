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
  , RequestReplyPair (..)

  , NodeName
  , DeviceName
  , DeviceAddress (..)

  , FromPid (..)
  , sendWithPid

  , Query (queryTarget, queryCommand)
  , mkQuery
  , QueryResult (unQueryResult)

  , Command
  , mkCommand
  , getCommand

  , mkSuccess
  , mkError

  , PortManager (..)
  , DeviceController (..)

  , EncodedDeviceState
  , encodeDeviceState
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

-- Devices have a state and a set of commands that can be sent to them.
-- ToDo: do we still need all those ToJSON and FromJSON instances?
-- ToDo: do we still need DeviceCommand in here or is it enough to use the RequestReplyPairs?
class ( ToJSON model, FromJSON model
      , ToJSON (DeviceState model), FromJSON (DeviceState model)
      , ToJSON (DeviceCommand model), FromJSON (DeviceCommand model)
      ) => IsDevice (model :: *) where
  -- the state of a device
  data family DeviceState model :: *

  -- the commands a device understands
  data family DeviceCommand model :: *

  -- a device controller runs a process for a device, takes commands and executes them
  startDeviceController :: model -> Slave -> PortManager -> Process DeviceController

-- Request-reply-pairs are used to describe requests to devices and their expected replies.
-- In a sense, we're tying those two together. Request-reply-pairs need to have instances
-- for Binary, so we can send them through Cloud Haskell, and WebSocketsData, so we can
-- send them over websockets. For the WebSocketsData, we're abusing ToJSOn and FromJSON instances.
-- ToDo: how can we enforce that only the intended request-reply-pairs are used?
-- ToDo: how would we go about replies that can be generated "out of nothing",
--       i.e. without a request that "generates" them?
class ( IsDevice model
      , Binary (Request model t), ToJSON (Request model t), FromJSON (Request model t), WebSocketsData (Request model t)
      , Binary (Reply   model t), ToJSON (Reply   model t), FromJSON (Reply   model t), WebSocketsData (Reply   model t)
      ) => RequestReplyPair model t where
  data family Request model t :: *
  data family Reply   model t :: *

-- Device states can be serialised and sent over the network. However, without
-- knowledge of the concrete device model at hand, the state can not be interpretet.
newtype EncodedDeviceState = EncodedDeviceState { getState :: Text } deriving (Generic, Binary, ToJSON, FromJSON)

encodeDeviceState :: IsDevice model => DeviceState model -> EncodedDeviceState
encodeDeviceState = EncodedDeviceState . decodeUtf8 . LBS.toStrict . encode

eitherDecodeDeviceState :: IsDevice model => EncodedDeviceState -> Either String (DeviceState model)
eitherDecodeDeviceState = eitherDecode' . LBS.fromStrict . encodeUtf8 . getState

instance WebSocketsData EncodedDeviceState where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

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
mkCommand = Command . decodeUtf8 . LBS.toStrict . encode

-- Turn the representation of a command into an actual command for a specific device.
getCommand :: IsDevice model => Command -> Either String (DeviceCommand model)
getCommand = eitherDecode' . LBS.fromStrict . encodeUtf8 . unCommand


-- A Query is intended for a specific device and carries a command. This command
-- must be of appropriate "type" in order for the device to be able to understand it.
data Query = Query { queryTarget  :: DeviceAddress
                   , queryCommand :: Command
                   }
  deriving (Generic, Binary, Typeable, ToJSON, FromJSON, Show)

instance WebSocketsData Query where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'

mkQuery :: IsDevice model => DeviceAddress -> DeviceCommand model -> Query
mkQuery deviceAddress command = Query deviceAddress (mkCommand command)


newtype QueryResult = QueryResult { unQueryResult :: Either Text Text } deriving (Generic, Binary, Typeable, ToJSON, FromJSON)

instance WebSocketsData QueryResult where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'


mkError :: Text -> QueryResult
mkError = QueryResult . Left

mkSuccess :: (ToJSON a, FromJSON a) => a -> QueryResult
mkSuccess = QueryResult . Right . decodeUtf8 . LBS.toStrict . encode

--------------------------------------------------------------------------------

data MiddlewareEvent = StateChangeEvent DeviceAddress EncodedDeviceState
  deriving (Generic, ToJSON, FromJSON)

instance WebSocketsData MiddlewareEvent where
  toLazyByteString = encode
  fromLazyByteString = fromJust . decode'
