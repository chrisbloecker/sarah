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
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
--------------------------------------------------------------------------------

instance HasRemote DHT22 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    temperature <- lift $ reactiveLabel "--°C"
    humidity    <- lift $ reactiveLabel "--%"

    getTemperatureButtonId <- newIdent
    getHumidityButtonId    <- newIdent

    let getTemperatureButton = H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                                        H.! A.id (H.toValue getTemperatureButtonId) $
                                   icon refresh
        getHumidityButton    = H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                                        H.! A.id (H.toValue getHumidityButtonId) $
                                   icon refresh

    let eventStateChangedHandler :: Handler (DeviceState DHT22)
        eventStateChangedHandler DHT22.SensorState{..} = case readings of
          Left (_ :: DHT22.Error) -> putStrLn "[DHT22.eventStateChangedHandler] An error occured when obtaining the readings"
          Right (Temperature t, Humidity h) -> do
            getHandler temperature (pack . show $ t)
            getHandler humidity    (pack . show $ h)

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDClick getTemperatureButtonId $ runRemote $
        withResponse DHT22.GetReadingsRequest
          doNothing
          (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)

    addPageAction $
      onElementIDClick getHumidityButtonId $ runRemote $
        withResponse DHT22.GetReadingsRequest
          doNothing
          (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)


    addPageTile $
      let title = unwords [deviceNode deviceAddress, deviceName deviceAddress]
      in mkTile title $ list [ listItem (H.text "Temperature") (H.div $ getItem temperature >> getTemperatureButton)
                             , listItem (H.text "Humidity")    (H.div $ getItem humidity >> getHumidityButton)
                             ]

    lift $ runRemote $
      withResponse DHT22.GetReadingsRequest
        doNothing
        (\(DHT22.GetReadingsReply state) -> eventStateChangedHandler state)
