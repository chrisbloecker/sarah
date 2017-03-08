{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.AC.Toshiba
  where
--------------------------------------------------------------------------------
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Widgets
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand)
import Sarah.Middleware.Device        (ToshibaAC)
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

instance HasRemote ToshibaAC where
  renderRemote appEnv deviceAddress _ = do
    (eventTemperature, handlerTemperature) <- liftIO newEvent
    behaviourTemperature                   <- stepper "--°C" eventTemperature
    displayTemperature                     <- reactiveLabel behaviourTemperature

    onButton       <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]
    offButton      <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-off" ]
    tempDownButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-chevron-left" ]
    tempUpButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-chevron-right" ]
    coolButton     <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-snowflake-o" ]
    dryButton      <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]
    fanButton      <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-cloud" ]
    normalButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-minus"]
    ecoButton      <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-leaf" ]
    hiButton       <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-fire" ]

    -- ToDo: get the state of the device and modify it, don't just overwrite the state
    on click onButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.PowerOn)
      handleResponse "[ToshibaAC.onButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click offButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.PowerOff)
      handleResponse "[ToshibaAC.offButton.click]" mres doNothing (\(_ :: ()) -> doNothing)
{-
    on click tempDownButton $ embedUI $ do
      mtemp <- sendCommand appEnv deviceAddress (mkCommand Toshiba.GetTemperature)
      handleResponse "[ToshibaAC.tempDownButton.click]" mtemp doNothing $ \(t :: Toshiba.Temperature) -> do
        mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.SetTemperature)
-}
    on click coolButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeCool)
      handleResponse "[ToshibaAC.coolButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click dryButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeDry)
      handleResponse "[ToshibaAC.dryButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click fanButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetMode Toshiba.ModeFan)
      handleResponse "[ToshibaAC.fanButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click normalButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetPowerMode Nothing)
      handleResponse "[Toshiba.normalButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click ecoButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetPowerMode (Just Toshiba.PowerEco))
      handleResponse "[ToshibaAC.ecoButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click hiButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand $ Toshiba.SetPowerMode (Just Toshiba.PowerHigh))
      handleResponse "[Toshiba.hiButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    div #+ [ p # set class_ "text-center"
               #+ map element [ onButton, offButton ]
           , p # set class_ "text-center"
               #+ [ element tempDownButton, element displayTemperature, element tempUpButton ]
           , p # set class_ "text-center"
               #+ map element [ coolButton, dryButton, fanButton ]
           , p # set class_ "text-center"
               #+ map element [ normalButton, ecoButton, hiButton ]
           ]
