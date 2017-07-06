{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (runReaderT, lift, ask)
import Data.Foldable                  (traverse_)
import Data.Text                      (unwords)
import Graphics.UI.Material
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
import qualified Text.Blaze.Html5                    as H
--------------------------------------------------------------------------------

instance HasRemote HS110 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    powerSwitch <- lift $ reactiveToggle False

    let eventStateChangedHandler :: Handler (DeviceState HS110)
        eventStateChangedHandler HS110.HS110State{..} = getHandler powerSwitch isOn

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDCheckedChange (getItemId powerSwitch) $ \state -> runRemote $
        withoutResponse $
          if state
            then HS110.PowerOn
            else HS110.PowerOff

    let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
        widget = mkTile title $
                     list [ listItem (H.text "Power") $ getItem powerSwitch ]

    addPageTile widget

    -- get the state of the device
    addPageAction $
      runRemote $
        withResponse HS110.GetStateRequest
        doNothing
        (\(HS110.GetStateReply state) -> eventStateChangedHandler state)
