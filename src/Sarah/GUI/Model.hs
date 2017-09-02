{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Model
  where
--------------------------------------------------------------------------------
import Control.Concurrent.STM      (TVar, atomically, modifyTVar)
import Control.Monad.Reader        (ReaderT, ask, lift)
import Data.Aeson                  (ToJSON, FromJSON)
import Data.HashMap.Strict         (HashMap)
import Data.Sequence               (Seq, (|>))
import Data.Text                   (Text, unpack)
import Graphics.UI.Threepenny.Core
import Network.HTTP.Client         (Manager)
import Raspberry.IP                (WebAddress)
import Servant.Client
import Sarah.Middleware
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
--------------------------------------------------------------------------------

-- ToDo: Tidings?
-- ToDo: wrap this into some type that represents device events? Maybe together
--       with a device representation, so we can deserialise it properly?
type RemoteEvent = (Event EncodedDeviceState, Handler EncodedDeviceState)

data AppEnv = AppEnv { middleware :: WebAddress }

type App = ReaderT AppEnv UI

type ErrorHandler     = IO ()
type SuccessHandler a = a -> IO ()

--------------------------------------------------------------------------------

class PageTileBuilder builder where
  addPageTile :: H.Html -> ReaderT builder UI ()

class PageActionBuilder builder where
  addPageAction :: UI () -> ReaderT builder UI ()

data RemoteBuilderEnv = RemoteBuilderEnv { appEnv             :: AppEnv
                                         , deviceAddress      :: DeviceAddress
                                         , eventStateChanged  :: Event EncodedDeviceState
                                         , remoteTiles        :: TVar (Seq H.Html)
                                         , pageActions        :: TVar (Seq (UI ()))
                                         , runRemote          :: RemoteRunner () -> UI ()
                                         }

instance PageTileBuilder RemoteBuilderEnv where
  addPageTile tile = do
    RemoteBuilderEnv{..} <- ask
    liftIO . atomically $ modifyTVar remoteTiles (|> tile)

instance PageActionBuilder RemoteBuilderEnv where
  addPageAction action = do
    RemoteBuilderEnv{..} <- ask
    liftIO . atomically $ modifyTVar pageActions (|> action)


data ScheduleBuilderEnv = ScheduleBuilderEnv { deviceAddress :: DeviceAddress
                                             , scheduleTiles :: TVar (Seq H.Html)
                                             , pageActions   :: TVar (Seq (UI ()))
                                             , getSchedule   :: ScheduleBuilder [Schedule]
                                             }

instance PageTileBuilder ScheduleBuilderEnv where
  addPageTile tile = do
    ScheduleBuilderEnv{..} <- ask
    liftIO . atomically $ modifyTVar scheduleTiles (|> tile)

instance PageActionBuilder ScheduleBuilderEnv where
  addPageAction action = do
    ScheduleBuilderEnv{..} <- ask
    liftIO . atomically $ modifyTVar pageActions (|> action)


type RemoteBuilder   = ReaderT RemoteBuilderEnv   UI
type ScheduleBuilder = ReaderT ScheduleBuilderEnv UI

-- for running remotes so they know what device they control and where to find it
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
  buildRemote :: model -> RemoteBuilder ()

  -- For generating a "widget" than can be used to customise schedules for a device.
  buildSchedule :: model -> ScheduleBuilder ()


doNothing :: IO ()
doNothing = return ()
