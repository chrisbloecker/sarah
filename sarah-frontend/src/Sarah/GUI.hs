{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI
  ( setup
  ) where
--------------------------------------------------------------------------------
import           Control.Monad                             (void)
import           Control.Monad.Except                      (ExceptT, runExceptT, liftIO)
import           Graphics.UI.Threepenny
import           Graphics.UI.Threepenny.Core
import           Prelude                            hiding (div)
import           Sarah.GUI.Model
import           Sarah.GUI.Widgets
import           Sarah.Middleware.Device.AC.Toshiba as AC
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Client as Middleware
--------------------------------------------------------------------------------

setup :: MiddlewareConfig -> Window -> UI ()
setup MiddlewareConfig{..} window = void $ do
  navbar  <- mkNavbar
  content <- runEIO $ Middleware.getStatus manager middleware

  getBody window #+ [ element navbar
                    , div # set id_ "content"
                          # set class_ "container"
                          #+ either (const []) renderStatus content
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

runEIO :: (MonadIO m) => ExceptT e IO a -> m (Either e a)
runEIO = liftIO . runExceptT
