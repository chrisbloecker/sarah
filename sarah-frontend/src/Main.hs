{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import           Control.Monad.Except                     (runExceptT, liftIO)
import           Graphics.UI.Threepenny.Core
import           Network.HTTP.Client                      (Manager, newManager, defaultManagerSettings)
import           Sarah.Middleware.Client
import           Sarah.Middleware.Device.AC.Toshiba as AC
import           Servant.Common.BaseUrl                   (BaseUrl (..))
import           Servant.Client                           (Scheme (Http))
--------------------------------------------------------------------------------
import qualified Graphics.UI.Threepenny      as UI
--------------------------------------------------------------------------------

data MiddlewareConfig = MiddlewareConfig { manager    :: Manager
                                         , middleware :: BaseUrl
                                         }

--------------------------------------------------------------------------------

setup :: MiddlewareConfig -> Window -> UI ()
setup MiddlewareConfig{..} window = do
  return window #
    set UI.title "Sarah"
  buttonOn  <- UI.button # set UI.text "On"
  buttonOff <- UI.button # set UI.text "Off"
  getBody window #+ [element buttonOn, element buttonOff]
  on UI.click buttonOn $ const $ do
    element buttonOn # set UI.text "ON!"
    liftIO . runExceptT $ runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing) manager middleware
  on UI.click buttonOff $ const $ do
    element buttonOff # set UI.text "OFF!"
    liftIO . runExceptT $ runAcServer (AC.Config AC.T20 AC.FanAuto AC.ModeOff Nothing) manager middleware

main :: IO ()
main = do
  let config = defaultConfig { jsPort   = Just 8023
                             , jsStatic = Just "webroot"
                             }
      middlewareHost = "localhost"
      middlewarePort = 8090
      middleware = BaseUrl Http middlewareHost middlewarePort ""
  manager <- newManager defaultManagerSettings
  startGUI config (setup MiddlewareConfig{..})
