{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import Control.Concurrent             (forkIO, threadDelay)
import Control.Monad                  (forever)
import Control.Monad.Reader           (runReaderT, lift, ask)
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
      (eventReadings, handlerReadings) <- liftIO newEvent
      behaviourReadings                <- stepper ("--°C", "--%") eventReadings

      temperatureDisplay <- reactiveLabel ((++ "°C") . fst <$> behaviourReadings)
      humidityDisplay    <- reactiveLabel ((++ "%")  . snd <$> behaviourReadings)

      let buttonClass = buildClass [ btn
                                   , btn_sm
                                   , btn_default
                                   , btn_circle
                                   , btn_no_background
                                   ]

      getTemperatureButton <- bootstrapButton buttonClass (Glyphicon "fa fa-thermometer-full")
      getHumidityButton    <- bootstrapButton buttonClass Glyph.tint

      let eventStateChangedHandler _ = flip runReaderT remoteRunnerEnv $ withResponse DHT22.GetReadings doNothing handlerReadings

      unregister <- liftIO $ register eventStateChanged eventStateChangedHandler

      on click getTemperatureButton $ embedUI $ notifyStateChanged ()
      on click getHumidityButton    $ embedUI $ notifyStateChanged ()

      div #+ [ p # set class_ "text-center"
                 #+ [ string "Temperature: ", element temperatureDisplay, element getTemperatureButton ]
             , p # set class_ "text-center"
                 #+ [ string "Humidity: ", element humidityDisplay, element getHumidityButton ]
             ]

      notifyStateChanged ()
