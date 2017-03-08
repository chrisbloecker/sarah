{-# LANGUAGE ScopedTypeVariables #-}

module Sarah.GUI.Remote.AC.Toshiba
  where

import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand)
import Sarah.Middleware.Device        (ToshibaAC)
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba


instance HasRemote ToshibaAC where
  renderRemote appEnv deviceAddress _ = do
    onButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]
    offButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-off" ]
    coolButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-snowflake-o" ]
    dryButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]
    fanButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-cloud" ]
    ecoButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-leaf" ]
    hiButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-fire" ]

    -- ToDo: get the state of the device and modify it, don't just overwrite the state
    on click onButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.PowerOn)
      handleResponse "[ToshibaAC.onButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click offButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand Toshiba.PowerOff)
      handleResponse "[ToshibaAC.offButton.click]" mres doNothing (\(_ :: ()) -> doNothing)

    on click coolButton $ const undefined
    on click dryButton $ const undefined
    on click fanButton $ const undefined
    on click ecoButton $ const undefined
    on click hiButton $ const undefined

    div #+ [ p # set class_ "text-center"
               #+ map element [ onButton, offButton ]
           , p # set class_ "text-center"
               #+ map element [ coolButton, dryButton, fanButton ]
           , p # set class_ "text-center"
               #+ map element [ ecoButton, hiButton ]
           ]
