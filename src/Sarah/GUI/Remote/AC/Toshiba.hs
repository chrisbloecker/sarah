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
import Data.Text                      (Text)
import Data.UUID                      (toString)
import Data.UUID.V4                   (nextRandom)
import Graphics.UI.Material
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand)
import Sarah.Middleware.Device        (ToshibaAC)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Material               as Material
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

instance HasRemote ToshibaAC where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      window <- askWindow

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

      onOffToggle  <- Material.reactiveToggle behaviourPowerSwitch

      displayTemperature <- reactiveLabel behaviourTemperature
      displayFanlevel    <- reactiveLabel behaviourFanlevel
      displayMode        <- reactiveLabel behaviourMode
      displayPowerMode   <- reactiveLabel behaviourPowerMode

      -- styles for differently coloured buttons
      let grey     = Material.empty
          accented = Material.mdl_color_text_accent

      tempDownButton <- button # set class_ (Material.unClass $ Material.buildClass [Material.mdl_button, Material.mdl_js_button]) #+ [ Material.icon Material.chevron_left]
      tempUpButton   <- button # set class_ (Material.unClass $ Material.buildClass [Material.mdl_button, Material.mdl_js_button]) #+ [ Material.icon Material.chevron_right]
      fanDownButton  <- button # set class_ (Material.unClass $ Material.buildClass [Material.mdl_button, Material.mdl_js_button]) #+ [ Material.icon Material.chevron_left]
      fanUpButton    <- button # set class_ (Material.unClass $ Material.buildClass [Material.mdl_button, Material.mdl_js_button]) #+ [ Material.icon Material.chevron_right]

      (eventAuto, handlerAuto) <- liftIO newEvent
      (eventCool, handlerCool) <- liftIO newEvent
      (eventDry,  handlerDry)  <- liftIO newEvent
      (eventFan,  handlerFan)  <- liftIO newEvent

      behaviourAuto <- stepper grey eventAuto
      behaviourCool <- stepper grey eventCool
      behaviourDry  <- stepper grey eventDry
      behaviourFan  <- stepper grey eventFan

      autoButton <- reactiveListItem behaviourAuto
      coolButton <- reactiveListItem behaviourCool
      dryButton  <- reactiveListItem behaviourDry
      fanButton  <- reactiveListItem behaviourFan

      element (getElement autoButton) # set text "Auto"
      element (getElement coolButton) # set text "Cool"
      element (getElement dryButton)  # set text "Dry"
      element (getElement fanButton)  # set text "Fan"

      (eventNormal, handlerNormal) <- liftIO newEvent
      (eventEco,    handlerEco)    <- liftIO newEvent
      (eventHi,     handlerHi)     <- liftIO newEvent

      behaviourNormal <- stepper grey eventNormal
      behaviourEco    <- stepper grey eventEco
      behaviourHi     <- stepper grey eventHi

      normalButton <- reactiveListItem behaviourNormal
      ecoButton    <- reactiveListItem behaviourEco
      hiButton     <- reactiveListItem behaviourHi

      element (getElement normalButton) # set text "Normal"
      element (getElement ecoButton)    # set text "Eco"
      element (getElement hiButton)     # set text "High"

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
                Temperature t -> handlerTemperature (show t ++ "°C")

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

      on checkedChange (Material.getCheckbox onOffToggle) $ \state -> liftIO $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write $ if state then Toshiba.PowerOn else Toshiba.PowerOff)
      on click         (getElement tempUpButton)                   $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.UpTemperature)
      on click         (getElement tempDownButton)                 $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.DownTemperature)
      on click         (getElement fanUpButton)                    $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.UpFan)
      on click         (getElement fanDownButton)                  $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write Toshiba.DownFan)
      on click         (getElement autoButton)                     $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeAuto))
      on click         (getElement coolButton)                     $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeCool))
      on click         (getElement dryButton)                      $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeDry))
      on click         (getElement fanButton)                      $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeFan))
      on click         (getElement normalButton)                   $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode Nothing))
      on click         (getElement ecoButton)                      $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode $ Just Toshiba.PowerEco))
      on click         (getElement hiButton)                       $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode $ Just Toshiba.PowerHigh))

      liftIO $ flip runReaderT remoteRunnerEnv $ withResponse (Toshiba.Read Toshiba.GetConfig) doNothing (\(Toshiba.DeviceState config) -> eventStateChangedHandler config)

      dropdownMode      <- Material.dropdown (element displayMode) (map element [autoButton, coolButton, dryButton, fanButton])
      dropdownPowerMode <- Material.dropdown (element displayPowerMode) (map element [normalButton, ecoButton, hiButton])

      getElement <$> Material.list [ Material.listItem (string "Power") (element onOffToggle)
                                   , Material.listItem (string "Temperature") (div #+ [ element tempDownButton, element displayTemperature, element tempUpButton ])
                                   , Material.listItem (string "Fan") (div #+ [ element fanDownButton, element displayFanlevel, element fanUpButton ])
                                   , Material.listItem (string "Mode") (element dropdownMode)
                                   , Material.listItem (string "Power Mode") (element dropdownPowerMode)
                                   ]
