{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Model
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader        (ReaderT)
import Graphics.UI.Threepenny.Core
import Network.HTTP.Client         (Manager)
import Servant.Common.BaseUrl      (BaseUrl)
import Sarah.Middleware            (IsDevice, DeviceAddress, Command, Query (..), QueryResult, runEIO, runDeviceCommand)
--------------------------------------------------------------------------------

data AppEnv = AppEnv { manager    :: Manager
                     , middleware :: BaseUrl
                     }

type AppT  m = ReaderT AppEnv m
type AppIO   = AppT IO

--------------------------------------------------------------------------------

-- models which have an instance of IsDevice can be extended with HasRemote.
class IsDevice model => HasRemote model where
  -- For generating a "widget" that can be used as a remote to control a device.
  -- We need some context though:
  --  - The AppEnv, so we know how to talk to the device
  --  - A DeviceAddress, so we know where the device is. Potentially, there can be
  --    many devices of the same kind available, even at the same node.
  renderRemote :: AppEnv -> DeviceAddress -> model -> UI Element

-- construct a command, build a query, and send it
sendCommand :: AppEnv -> DeviceAddress -> Command -> IO (Maybe QueryResult)
sendCommand AppEnv{..} deviceAddress command = do
  let query = Query deviceAddress command
  mres <- runEIO $ runDeviceCommand query manager middleware
  case mres of
    Left  err -> putStrLn ("[sendCommand] " ++ show err) >> return Nothing
    Right res -> return (Just res)

-- just a liftIO for UI
embedUI :: IO a -> b -> UI a
embedUI = const . liftIO
