{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.AC.Toshiba
  where
--------------------------------------------------------------------------------
import Control.Monad                  (unless)
import Control.Monad.Reader           (runReaderT, lift, ask)
import Data.Foldable                  (traverse_)
import Data.Functor.Contravariant     ((>$), contramap)
import Data.Text                      (Text, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, unwords)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand, DeviceAddress (..))
import Sarah.Middleware.Device        (ToshibaAC)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Material               as Material
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

instance HasRemote ToshibaAC where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    let emptyTemperature = "--"
        emptyFanlevel    = "--"

    (eventPowerSwitch, handlerPowerSwitch) <- liftIO newEvent
    (eventTemperature, handlerTemperature) <- liftIO newEvent
    (eventFanlevel,    handlerFanlevel)    <- liftIO newEvent
    (eventMode,        handlerMode)        <- liftIO newEvent
    (eventPowerMode,   handlerPowerMode)   <- liftIO newEvent

    behaviourPowerSwitch <- stepper False eventPowerSwitch
    behaviourTemperature <- stepper emptyTemperature eventTemperature
    behaviourFanlevel    <- stepper emptyFanlevel eventFanlevel
    behaviourMode        <- stepper "Off" eventMode
    behaviourPowerMode   <- stepper "Normal" eventPowerMode

    (toggle, toggleId) <- lift $ Material.reactiveToggle behaviourPowerSwitch

    displayTemperature <- lift $ reactiveLabel behaviourTemperature
    displayFanlevel    <- lift $ reactiveLabel behaviourFanlevel
    displayMode        <- lift $ reactiveLabel behaviourMode
    displayPowerMode   <- lift $ reactiveLabel behaviourPowerMode

    -- styles for differently coloured buttons
    let grey     = ""
        accented = "mdl-color-text--accent"

    tempDownButtonId <- newIdent
    tempUpButtonId   <- newIdent
    fanDownButtonId  <- newIdent
    fanUpButtonId    <- newIdent

    let tempDownButton = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue tempDownButtonId) $
                             Material.icon Material.chevron_left
        tempUpButton   = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue tempUpButtonId) $
                             Material.icon Material.chevron_right
        fanDownButton  = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue fanDownButtonId) $
                             Material.icon Material.chevron_left
        fanUpButton    = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue fanUpButtonId) $
                             Material.icon Material.chevron_right

    (eventAuto, handlerAuto) <- liftIO newEvent
    (eventCool, handlerCool) <- liftIO newEvent
    (eventDry,  handlerDry)  <- liftIO newEvent
    (eventFan,  handlerFan)  <- liftIO newEvent

    behaviourAuto <- stepper grey eventAuto
    behaviourCool <- stepper grey eventCool
    behaviourDry  <- stepper grey eventDry
    behaviourFan  <- stepper grey eventFan

    (autoButton, autoButtonId) <- lift $ reactiveListItem "Auto" behaviourAuto
    (coolButton, coolButtonId) <- lift $ reactiveListItem "Cool" behaviourCool
    (dryButton,  dryButtonId)  <- lift $ reactiveListItem "Dry"  behaviourDry
    (fanButton,  fanButtonId)  <- lift $ reactiveListItem "Fan"  behaviourFan

    (eventNormal, handlerNormal) <- liftIO newEvent
    (eventEco,    handlerEco)    <- liftIO newEvent
    (eventHi,     handlerHi)     <- liftIO newEvent

    behaviourNormal <- stepper grey eventNormal
    behaviourEco    <- stepper grey eventEco
    behaviourHi     <- stepper grey eventHi

    (normalButton, normalButtonId) <- lift $ reactiveListItem "Normal" behaviourNormal
    (ecoButton,    ecoButtonId)    <- lift $ reactiveListItem "Eco"    behaviourEco
    (hiButton,     hiButtonId)     <- lift $ reactiveListItem "High"   behaviourHi

    let eventStateChangedHandler :: Handler (Toshiba.DeviceState ToshibaAC)
        eventStateChangedHandler Toshiba.Config{..} =
          if mode == Toshiba.ModeOff
            then do
              handlerPowerSwitch False
              handlerTemperature  emptyTemperature
              handlerFanlevel     emptyFanlevel
              handlerAuto grey
              handlerCool grey
              handlerDry  grey
              handlerFan  grey
              handlerEco  grey
              handlerHi   grey

          else do
            handlerPowerSwitch True

            case temperature of
              Temperature t -> handlerTemperature (pack $ show t ++ "°C")

            handlerFanlevel $ case fan of
              Toshiba.FanAuto     -> "Auto"
              Toshiba.FanQuiet    -> "Quiet"
              Toshiba.FanVeryLow  -> "Very Low"
              Toshiba.FanLow      -> "Low"
              Toshiba.FanNormal   -> "Normal"
              Toshiba.FanHigh     -> "High"
              Toshiba.FanVeryHigh -> "Very High"

            case mode of
              Toshiba.ModeAuto -> handlerAuto accented >> handlerCool grey     >> handlerDry grey     >> handlerFan  grey     >> handlerMode "Auto"
              Toshiba.ModeCool -> handlerAuto grey     >> handlerCool accented >> handlerDry grey     >> handlerFan  grey     >> handlerMode "Cool"
              Toshiba.ModeDry  -> handlerAuto grey     >> handlerCool grey     >> handlerDry accented >> handlerFan  grey     >> handlerMode "Dry"
              Toshiba.ModeFan  -> handlerAuto grey     >> handlerCool grey     >> handlerDry grey     >> handlerFan  accented >> handlerMode "Fan"
              Toshiba.ModeOff  -> handlerAuto grey     >> handlerCool grey     >> handlerDry grey     >> handlerFan  grey     >> handlerMode "Off"

            handlerEco grey
            handlerHi  grey
            case mpower of
              Nothing                -> handlerPowerMode "Normal"
              Just Toshiba.PowerEco  -> handlerEco accented >> handlerPowerMode "Eco"
              Just Toshiba.PowerHigh -> handlerHi  accented >> handlerPowerMode "High"

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $ onElementIDCheckedChange toggleId $ \state -> liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write $ if state then Toshiba.PowerOn else Toshiba.PowerOff)
    addPageAction $ onElementIDClick         tempUpButtonId $     liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.UpTemperature)
    addPageAction $ onElementIDClick         tempDownButtonId $   liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.DownTemperature)
    addPageAction $ onElementIDClick         fanUpButtonId $      liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.UpFan)
    addPageAction $ onElementIDClick         fanDownButtonId $    liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.DownFan)
    addPageAction $ onElementIDClick         autoButtonId $       liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeAuto))
    addPageAction $ onElementIDClick         coolButtonId $       liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeCool))
    addPageAction $ onElementIDClick         dryButtonId $        liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeDry))
    addPageAction $ onElementIDClick         fanButtonId $        liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeFan))
    addPageAction $ onElementIDClick         normalButtonId $     liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode Nothing))
    addPageAction $ onElementIDClick         ecoButtonId $        liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode $ Just Toshiba.PowerEco))
    addPageAction $ onElementIDClick         hiButtonId $         liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode $ Just Toshiba.PowerHigh))

    liftIO $ flip runReaderT remoteRunnerEnv $ withResponse (Toshiba.Read Toshiba.GetConfig) doNothing (\(Toshiba.DeviceState config) -> eventStateChangedHandler config)

    dropdownMode      <- lift $ Material.dropdown displayMode      [autoButton, coolButton, dryButton, fanButton]
    dropdownPowerMode <- lift $ Material.dropdown displayPowerMode [normalButton, ecoButton, hiButton]

    let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
        widget = Material.mkTile title $
                     Material.list [ Material.listItem (H.text "Power") toggle
                                   , Material.listItem (H.text "Temperature") (H.div $ do
                                                                                   tempDownButton
                                                                                   displayTemperature
                                                                                   tempUpButton)
                                   , Material.listItem (H.text "Fan") (H.div $ do
                                                                           fanDownButton
                                                                           displayFanlevel
                                                                           fanUpButton)
                                   , Material.listItem (H.text "Mode") dropdownMode
                                   , Material.listItem (H.text "Power Mode") dropdownPowerMode
                                   ]

    addPageTile widget
