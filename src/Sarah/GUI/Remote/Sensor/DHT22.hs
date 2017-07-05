{-# LANGUAGE OverloadedStrings   #-}
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
import Data.Text                      (Text, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (unwords)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Websocket            (withResponse)
import Sarah.Middleware               (DeviceState, EncodedDeviceState, decodeDeviceState, mkCommand, DeviceAddress (..))
import Sarah.Middleware.Device        (DHT22)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.Sensor.DHT22 as DHT22
import qualified Graphics.UI.Material                 as Material
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
--------------------------------------------------------------------------------

instance HasRemote DHT22 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    (eventReadings, handlerReadings) <- liftIO newEvent
    behaviourReadings                <- stepper ("--", "--") eventReadings

    temperatureDisplay <- lift $ Material.reactiveLabel (pack . (++ "°C") . fst <$> behaviourReadings)
    humidityDisplay    <- lift $ Material.reactiveLabel (pack . (++ "%")  . snd <$> behaviourReadings)

    getTemperatureButtonId <- newIdent
    getHumidityButtonId    <- newIdent

    let getTemperatureButton = H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                                        H.! A.id (H.toValue getTemperatureButtonId) $
                                   Material.icon Material.refresh
        getHumidityButton    = H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                                        H.! A.id (H.toValue getHumidityButtonId) $
                                   Material.icon Material.refresh

    let eventStateChangedHandler :: Handler (DeviceState DHT22)
        eventStateChangedHandler DHT22.SensorState{..} = case readings of
          Left (_ :: DHT22.Error) -> putStrLn "[DHT22.eventStateChangedHandler] An error occured when obtaining the readings"
          Right (Temperature t, Humidity h) -> handlerReadings (show t, show h)

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDClick getTemperatureButtonId $ liftIO $ flip runReaderT remoteRunnerEnv $
        withResponse DHT22.GetReadingsRequest
          doNothing
          (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)

    addPageAction $
      onElementIDClick getHumidityButtonId $ liftIO $ flip runReaderT remoteRunnerEnv $
        withResponse DHT22.GetReadingsRequest
          doNothing
          (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)

    liftIO $ flip runReaderT remoteRunnerEnv $ withResponse DHT22.GetReadingsRequest doNothing (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)

    let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
        widget = Material.mkTile title $
                     Material.list [ Material.listItem (H.text "Temperature") (H.div $ do
                                                                                   temperatureDisplay
                                                                                   getTemperatureButton)
                                   , Material.listItem (H.text "Humidity") (H.div $ do
                                                                                humidityDisplay
                                                                                getHumidityButton)
                                   ]

    addPageTile widget
