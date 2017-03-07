module Sarah.GUI.Remote.Example
  where

import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), sendCommand, embedUI)
import Sarah.Middleware               (QueryResult (..), Result (..), mkCommand)
import Sarah.Middleware.Device        (ExampleDevice)
import qualified Sarah.Middleware.Device.Example as ExampleDevice


instance HasRemote ExampleDevice where
  renderRemote appEnv deviceAddress _ = do
    getRandomNumberButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-random" ]
    alwaysFailingButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]

    on click getRandomNumberButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetRandomNumber)
      case mres of
        Nothing -> putStrLn "[ExampleDevice.getRandomNumberButton.click] No response"
        Just (QueryResult result) -> case result of
          Error   message -> putStrLn $ "[ExampleDevice.getRandomNumberButton.click] Error: " ++ show message
          Success result  -> putStrLn $ "[ExampleDevice.getRandomNumberButton.click] Success: " ++ decodeWrapped result

    on click alwaysFailingButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetRandomNumber)
      case mres of
        Nothing  -> putStrLn "[ExampleDevice.alwaysFailingButton.click] No response"
        Just res -> putStrLn "[ExampleDevice.alwaysFailingButton.click] Got response"

    div #+ [ p # set class_ "text-center"
               #+ map element [getRandomNumberButton, alwaysFailingButton ]
           ]
