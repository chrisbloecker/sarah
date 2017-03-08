{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Example
  where
--------------------------------------------------------------------------------
import Data.Text                      (Text)
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), sendCommand, embedUI, handleResponse, doNothing)
import Sarah.GUI.Widgets              (reactiveLabel)
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand, decodeWrapped)
import Sarah.Middleware.Device        (ExampleDevice)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.Example as ExampleDevice
--------------------------------------------------------------------------------

instance HasRemote ExampleDevice where
  renderRemote appEnv deviceAddress _ = do
    (eventDisplay, handlerDisplay) <- liftIO newEvent
    behaviourDisplay <- stepper "foo" eventDisplay

    display <- reactiveLabel behaviourDisplay

    getRandomNumberButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-random" ]
    alwaysFailingButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]

    on click getRandomNumberButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetRandomNumber)
      handleResponse "[ExampleDevice.getRandomNumberButton.click]" mres doNothing (\(x :: Integer) -> handlerDisplay $ show x)

    on click alwaysFailingButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.AlwaysFailing)
      handleResponse "[ExampleDevice.alwaysFailingButton.click]" mres doNothing (const doNothing :: Text -> IO ())

    div #+ [ p # set class_ "text-center"
               #+ [ element display ]
           , p # set class_ "text-center"
               #+ map element [getRandomNumberButton, alwaysFailingButton ]
           ]
