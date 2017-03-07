module Sarah.GUI.Remote.Sensor.DHT22
  where

import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Sarah.GUI.Model                (HasRemote (..), sendCommand, embedUI)
import Sarah.Middleware               (mkCommand)
import Sarah.Middleware.Device        (DHT22)
import qualified Sarah.Middleware.Device.Sensor.DHT22 as DHT22

instance HasRemote DHT22 where
  renderRemote appEnv deviceAddress _ = do
    readTemperatureButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-thermometer-full" ]
    readHumidityButton    <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]

    on click readTemperatureButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand DHT22.GetTemperature)
      case mres of
        Nothing  -> putStrLn "[DHT22.readTemperatureButton.click] No response"
        Just res -> putStrLn "[DHT22.readTemperatureButton.click] Got response"

    on click readHumidityButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand DHT22.GetHumidity)
      case mres of
        Nothing  -> putStrLn "[DHT22.readHumidityButton.click] No response"
        Just res -> putStrLn "[DHT22.readHumidityButton.click] Got response"

    div #+ [ p # set class_ "text-center"
               #+ map element [ readTemperatureButton, readHumidityButton ]
           ]
