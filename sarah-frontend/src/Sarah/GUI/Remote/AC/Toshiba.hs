{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.AC.Toshiba
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (lift, ask)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Widgets
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand)
import Sarah.Middleware.Device        (ToshibaAC)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Bootstrap.Glyphicon    as Glyph
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

instance HasRemote ToshibaAC where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      let emptyDisplay = "--°C"

      (eventDisplay, handlerDisplay) <- liftIO newEvent
      behaviourDisplay               <- stepper emptyDisplay eventDisplay
      display                        <- reactiveLabel behaviourDisplay

      -- differently coloured buttons
      let whiteButton = buildClass [ btn, btn_sm, btn_default, btn_circle, btn_no_background ]
          blueButton  = buildClass [ btn, btn_sm, btn_primary, btn_circle, btn_no_background ]
          greenButton = buildClass [ btn, btn_sm, btn_success, btn_circle, btn_no_background ]
          redButton   = buildClass [ btn, btn_sm, btn_danger,  btn_circle, btn_no_background ]

      onButton       <- button # set class_ (unClass whiteButton) #+ [ label # set text "On"  ]
      offButton      <- button # set class_ (unClass whiteButton) #+ [ label # set text "Off" ]
      tempDownButton <- bootstrapButton whiteButton Glyph.chevron_left
      tempUpButton   <- bootstrapButton whiteButton Glyph.chevron_right

      (eventAuto, handlerAuto) <- liftIO newEvent
      (eventCool, handlerCool) <- liftIO newEvent
      (eventDry,  handlerDry)  <- liftIO newEvent
      (eventFan,  handlerFan)  <- liftIO newEvent

      behaviourAuto <- stepper whiteButton eventAuto
      behaviourCool <- stepper whiteButton eventCool
      behaviourDry  <- stepper whiteButton eventDry
      behaviourFan  <- stepper whiteButton eventFan

      autoButton <- reactiveButton behaviourAuto (pure $ Style [])
      coolButton <- reactiveButton behaviourCool (pure $ Style [])
      dryButton  <- reactiveButton behaviourDry  (pure $ Style [])
      fanButton  <- reactiveButton behaviourFan  (pure $ Style [])

      element (getElement autoButton) #+ [ span # set class_ (unGlyphicon Glyph.font)  ]
      element (getElement coolButton) #+ [ span # set class_ "fa fa-snowflake-o"       ]
      element (getElement dryButton)  #+ [ span # set class_ (unGlyphicon Glyph.tint)  ]
      element (getElement fanButton)  #+ [ span # set class_ (unGlyphicon Glyph.cloud) ]

      (eventNormal, handlerNormal) <- liftIO newEvent
      (eventEco,    handlerEco)    <- liftIO newEvent
      (eventHi,     handlerHi)     <- liftIO newEvent

      behaviourNormal <- stepper whiteButton eventNormal
      behaviourEco    <- stepper whiteButton eventEco
      behaviourHi     <- stepper whiteButton eventHi

      normalButton <- reactiveButton behaviourNormal (pure $ Style [])
      ecoButton    <- reactiveButton behaviourEco    (pure $ Style [])
      hiButton     <- reactiveButton behaviourHi     (pure $ Style [])

      element (getElement normalButton) #+ [ span # set class_ (unGlyphicon Glyph.minus) ]
      element (getElement ecoButton)    #+ [ span # set class_ (unGlyphicon Glyph.leaf)  ]
      element (getElement hiButton)     #+ [ span # set class_ (unGlyphicon Glyph.fire)  ]

      let resetDisplay = handlerDisplay emptyDisplay

          resetMode = do
            handlerAuto whiteButton
            handlerCool whiteButton
            handlerDry  whiteButton
            handlerFan  whiteButton

          resetPower = do
            handlerEco whiteButton
            handlerHi  whiteButton

          resetRemote = do
            resetDisplay
            resetMode
            resetPower

      on click onButton $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.PowerOn)
        handleResponse "[ToshibaAC.onButton.click]" mres doNothing $ \Toshiba.Config{..} -> do
          resetRemote
          case mode of
            Toshiba.ModeAuto -> handlerAuto blueButton
            Toshiba.ModeCool -> handlerCool blueButton
            Toshiba.ModeDry  -> handlerDry  blueButton
            Toshiba.ModeFan  -> handlerFan  blueButton
            Toshiba.ModeOff  -> return ()

      on click offButton $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.PowerOff)
        handleResponse "[ToshibaAC.offButton.click]" mres doNothing $ \(_ :: ()) -> resetRemote

      on click (getElement autoButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeAuto)
        handleResponse "[ToshibaAC.autoButton.click]" mres doNothing $ \(_ :: ()) -> do
          resetMode
          handlerAuto blueButton

      on click (getElement tempUpButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.UpTemperature)
        handleResponse "[ToshibaAC.tempUpButton.click]" mres doNothing $ \(Temperature t) -> handlerDisplay (show t ++ "°C")

      on click (getElement tempDownButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.DownTemperature)
        handleResponse "[ToshibaAC.tempDownButton.click]" mres doNothing $ \(Temperature t) -> handlerDisplay (show t ++ "°C")

      on click (getElement coolButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeCool)
        handleResponse "[ToshibaAC.coolButton.click]" mres doNothing $ \(_ :: ()) -> do
          resetMode
          handlerCool blueButton

      on click (getElement dryButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeDry)
        handleResponse "[ToshibaAC.dryButton.click]" mres doNothing $ \(_ :: ()) -> do
          resetMode
          handlerDry blueButton

      on click (getElement fanButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeFan)
        handleResponse "[ToshibaAC.fanButton.click]" mres doNothing $ \(_ :: ()) -> do
          resetMode
          handlerFan  blueButton

      on click (getElement normalButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetPowerMode Nothing)
        handleResponse "[Toshiba.normalButton.click]" mres doNothing $ \(_ :: ()) -> resetPower

      on click (getElement ecoButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetPowerMode (Just Toshiba.PowerEco))
        handleResponse "[ToshibaAC.ecoButton.click]" mres doNothing $ \(_ :: ()) -> do
          resetPower
          handlerEco greenButton

      on click (getElement hiButton) $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetPowerMode (Just Toshiba.PowerHigh))
        handleResponse "[Toshiba.hiButton.click]" mres doNothing $ \(_ :: ()) -> do
          resetPower
          handlerHi redButton

      div #+ [ p # set class_ "text-center"
                 #+ map element [ onButton, offButton ]
             , p # set class_ "text-center"
                 #+ [ element tempDownButton, element display, element tempUpButton ]
             , p # set class_ "text-center"
                 #+ map element [ autoButton, coolButton, dryButton, fanButton ]
             , p # set class_ "text-center"
                 #+ map element [ normalButton, ecoButton, hiButton ]
             ]
