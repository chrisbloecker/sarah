module Sarah.GUI.Remote.AC.Toshiba
  where

import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), sendCommand, embedUI)
import Sarah.Middleware               (mkCommand)
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
    on click onButton   $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click offButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeOff  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click coolButton $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click dryButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeDry  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click fanButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeFan  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click ecoButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeAuto (Just AC.PowerEco))  (appEnv^.manager) (appEnv^.middleware)
    on click hiButton   $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool (Just AC.PowerHigh)) (appEnv^.manager) (appEnv^.middleware)

    div #+ [ p # set class_ "text-center"
               #+ map element [ onButton, offButton ]
           , p # set class_ "text-center"
               #+ map element [ coolButton, dryButton, fanButton ]
           , p # set class_ "text-center"
               #+ map element [ ecoButton, hiButton ]
           ]
