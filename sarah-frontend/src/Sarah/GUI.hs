{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI
  ( setup
  ) where
--------------------------------------------------------------------------------
import           Control.Monad                            (void)
import           Control.Monad.Except                     (ExceptT, runExceptT, liftIO)
import           Graphics.UI.Threepenny.Core
import           Sarah.GUI.Model
import           Sarah.GUI.Templates
import           Sarah.Middleware.Client
import           Sarah.Middleware.Device.AC.Toshiba as AC
--------------------------------------------------------------------------------
import qualified Graphics.UI.Threepenny      as UI
--------------------------------------------------------------------------------

setup :: MiddlewareConfig -> Window -> UI ()
setup MiddlewareConfig{..} window = void $ do
  navbar <- mkNavbar
  getBody window #+ [element navbar]

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
