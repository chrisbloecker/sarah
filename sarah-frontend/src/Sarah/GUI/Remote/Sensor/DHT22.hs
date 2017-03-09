{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (lift, ask)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Widgets
import Sarah.Middleware               (mkCommand)
import Sarah.Middleware.Device        (DHT22)
--------------------------------------------------------------------------------
import qualified Graphics.UI.Bootstrap.Glyphicon      as Glyph
import qualified Sarah.Middleware.Device.Sensor.DHT22 as DHT22
--------------------------------------------------------------------------------

instance HasRemote DHT22 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      (eventTemperature, handlerTemperature) <- liftIO newEvent
      (eventHumidity,    handlerHumidity)    <- liftIO newEvent

      behaviourTemperature <- stepper "--°C" eventTemperature
      behaviourHumidity    <- stepper "--%"  eventHumidity

      temperatureDisplay <- reactiveLabel behaviourTemperature
      humidityDisplay    <- reactiveLabel behaviourHumidity

      let buttonClass = buildClass [ btn, btn_sm, btn_default, btn_circle, btn_no_background ]

      getTemperatureButton <- bootstrapButton buttonClass (Glyphicon "fa fa-thermometer-full")
      getHumidityButton    <- bootstrapButton buttonClass Glyph.tint

      on click getTemperatureButton $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand DHT22.GetTemperature)
        handleResponse "[DHT22.getTemperatureButton.click]" mres doNothing (\(Temperature t) -> handlerTemperature $ show t ++ "°C")

      on click getHumidityButton $ embedUI $ do
        mres <- sendCommand appEnv deviceAddress (mkCommand DHT22.GetHumidity)
        handleResponse "[DHT22.getHumidityButton.click]" mres doNothing (\(Humidity h) -> handlerHumidity $ show h ++ "%")

      div #+ [ p # set class_ "text-center"
                 #+ [ element temperatureDisplay, element getTemperatureButton ]
             , p # set class_ "text-center"
                 #+ [ element humidityDisplay, element getHumidityButton ]
             ]
