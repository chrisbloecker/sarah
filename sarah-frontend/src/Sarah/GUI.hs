{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI
  ( setup
  ) where
--------------------------------------------------------------------------------
import           Control.Lens                       hiding ((#), div, element, set)
import           Control.Monad                             (void, mapM_)
import           Control.Monad.Except                      (ExceptT, runExceptT, liftIO)
import           Graphics.UI.Threepenny             hiding (map)
import           Graphics.UI.Threepenny.Core
import           Prelude                            hiding (div)
import           Sarah.GUI.Model
import           Sarah.GUI.Widgets
import           Sarah.Middleware.Device.AC.Toshiba as AC
import           Sarah.Middleware                          (Status (..), runEIO)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Client as Middleware
--------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup appEnv window = void $ do
  (remotesLink, devicesLink, navbar)  <- mkNavbar

  on click remotesLink $ \_ -> do
    mapM_ delete =<< getElementById window "content"
    devices <- runEIO $ Middleware.getStatus (appEnv^.manager) (appEnv^.middleware)
    getBody window #+ [ div # set id_ "content"
                            # set class_ "container"
                            #+ either (const []) (\Status{..} -> renderRemotes appEnv connectedNodes) devices
                      ]

  on click devicesLink $ \_ -> do
    mapM_ delete =<< getElementById window "content"
    status <- runEIO $ Middleware.getStatus (appEnv^.manager) (appEnv^.middleware)
    liftIO $ print status
    getBody window #+ [ div # set id_ "content"
                            # set class_ "container"
                            #+ either (const []) renderStatus status
                      ]

  getBody window #+ [ element navbar
                    , div # set id_ "content"
                    ]

{-
  buttonOn  <- UI.button # set UI.text "On"
  buttonOff <- UI.button # set UI.text "Off"
  getBody window #+ [element buttonOn, element buttonOff]
  on UI.click buttonOn $ const $ do
    element buttonOn # set UI.text "ON!"
    runEIO  $ runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing) manager middleware
  on UI.click buttonOff $ const $ do
    element buttonOff # set UI.text "OFF!"
    runEIO $ runAcServer (AC.Config AC.T20 AC.FanAuto AC.ModeOff Nothing) manager middleware
-}
