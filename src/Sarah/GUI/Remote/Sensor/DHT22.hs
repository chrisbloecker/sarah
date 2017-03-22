{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import Control.Concurrent             (forkIO, threadDelay)
import Control.Monad                  (forever)
import Control.Monad.Reader           (runReaderT, lift, ask)
import Data.Foldable                  (traverse_)
import Data.Text                      (Text)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, div)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Widgets
import Sarah.GUI.Websocket            (withResponse)
import Sarah.Middleware               (DeviceState, EncodedDeviceState, decodeDeviceState, mkCommand)
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
      behaviourReadings                <- stepper ("--", "--") eventReadings

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

      let eventStateChangedHandler :: Handler (DeviceState DHT22)
          eventStateChangedHandler DHT22.SensorState{..} = case readings of
            Left (_ :: DHT22.Error) -> putStrLn "[DHT22.eventStateChangedHandler] An error occured when obtaining the readings"
            Right (Temperature t, Humidity h) -> handlerReadings (show t, show h)

      unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

      on click getTemperatureButton $ embedUI $ flip runReaderT remoteRunnerEnv $ withResponse DHT22.GetReadingsRequest doNothing (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)
      on click getHumidityButton    $ embedUI $ flip runReaderT remoteRunnerEnv $ withResponse DHT22.GetReadingsRequest doNothing (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)

      liftIO $ flip runReaderT remoteRunnerEnv $ withResponse DHT22.GetReadingsRequest doNothing (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)

      div #+ [ p # set class_ "text-center"
                 #+ [ string "Temperature: ", element temperatureDisplay, element getTemperatureButton ]
             , p # set class_ "text-center"
                 #+ [ string "Humidity: ", element humidityDisplay, element getHumidityButton ]
             ]
