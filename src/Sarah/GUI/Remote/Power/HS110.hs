{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (runReaderT, lift, ask)
import Data.Foldable                  (traverse_)
import Data.Text                      (unwords)
import Graphics.UI.Threepenny  hiding (map)
import Graphics.UI.Threepenny.Core    (ffi, runFunction)
import Prelude                 hiding (div, unwords)
import Sarah.GUI.Model
import Sarah.GUI.Reactive             (onElementIDCheckedChange)
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (DeviceState, decodeDeviceState, DeviceAddress (..))
import Sarah.Middleware.Device        (HS110)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.Power.HS110 as HS110
import qualified Graphics.UI.Material                as Material
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5                    as H
--------------------------------------------------------------------------------
instance HasRemote HS110 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    (eventPowerSwitch, handlerPowerSwitch) <- liftIO newEvent
    behaviourPowerSwitch                   <- stepper False eventPowerSwitch

    (toggle, toggleId) <- lift $ Material.reactiveToggle behaviourPowerSwitch

    let eventStateChangedHandler :: Handler (DeviceState HS110)
        eventStateChangedHandler HS110.HS110State{..} = do
          putStrLn $ "[HS110.eventStateChangedHandler] " ++ show isOn
          handlerPowerSwitch isOn

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDCheckedChange toggleId $ \state -> liftIO $ flip runReaderT remoteRunnerEnv $
        withoutResponse $
          if state
            then HS110.PowerOn
            else HS110.PowerOff

    liftIO $ flip runReaderT remoteRunnerEnv $
      withResponse HS110.GetStateRequest
      doNothing
      (\(HS110.GetStateReply state) -> eventStateChangedHandler state)

    let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
        widget = Material.mkTile title $
                     Material.list [ Material.listItem (H.text "Power") toggle ]

    addPageTile widget
