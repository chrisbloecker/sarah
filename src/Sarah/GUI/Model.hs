{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Model
  where
--------------------------------------------------------------------------------
import Control.Concurrent.STM      (TVar)
import Control.Monad.Reader        (ReaderT, ask, lift)
import Data.Aeson                  (ToJSON, FromJSON)
import Data.HashMap.Strict         (HashMap)
import Data.Text                   (Text, unpack)
import Graphics.UI.Threepenny.Core
import Network.HTTP.Client         (Manager)
import Raspberry.IP                (WebAddress)
import Servant.Client
import Sarah.Middleware
--------------------------------------------------------------------------------

-- ToDo: Tidings?
-- ToDo: wrap this into some type that represents device events? Maybe together
--       with a device representation, so we can deserialise it properly?
type RemoteEvent = (Event EncodedDeviceState, Handler EncodedDeviceState)

data AppEnv = AppEnv { remoteEvents :: TVar (HashMap DeviceAddress RemoteEvent)
                     , counter      :: TVar Integer
                     , middleware   :: WebAddress
                     }

type App = ReaderT AppEnv UI

type ErrorHandler     = IO ()
type SuccessHandler a = a -> IO ()

--------------------------------------------------------------------------------

data RemoteBuilderEnv = RemoteBuilderEnv { appEnv             :: AppEnv
                                         , deviceAddress      :: DeviceAddress
                                         , eventStateChanged  :: Event EncodedDeviceState
                                         , remoteRunnerEnv    :: RemoteRunnerEnv
                                         }

type RemoteBuilder = ReaderT RemoteBuilderEnv UI

data RemoteRunnerEnv = RemoteRunnerEnv { deviceAddress :: DeviceAddress
                                       , middleware    :: WebAddress
                                       }

type RemoteRunner = ReaderT RemoteRunnerEnv IO

-- models which have an instance of IsDevice can be extended with HasRemote.
class IsDevice model => HasRemote model where
  -- For generating a "widget" that can be used as a remote to control a device.
  -- We need some context though:
  --  - The AppEnv, so we know how to talk to the device
  --  - A DeviceAddress, so we know where the device is. Potentially, there can be
  --    many devices of the same kind available, even at the same node.
  buildRemote :: model -> RemoteBuilder Element

doNothing :: IO ()
doNothing = return ()

-- just a liftIO for UI
embedUI :: IO a -> b -> UI a
embedUI = const . liftIO
