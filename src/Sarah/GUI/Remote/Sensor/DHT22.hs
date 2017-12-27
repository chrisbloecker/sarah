{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Sensor.DHT22
  where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Reader                 (lift, ask)
import Data.Foldable                        (traverse_)
import Data.Text                            (Text, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny               (Handler, register)
import Prelude                       hiding (unwords)
import Physics                              (Temperature (..), Humidity (..))
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Websocket                  (withResponse)
import Sarah.Middleware                     (DeviceState, EncodedDeviceState, decodeDeviceState, mkCommand, DeviceAddress (..))
import Sarah.Middleware.Device.Sensor.DHT22
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
--------------------------------------------------------------------------------

instance HasRemote DHT22 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    temperature <- lift $ reactiveLabel "--°C"
    humidity    <- lift $ reactiveLabel "--%"

    getTemperatureButton <- iconButton refresh
    getHumidityButton    <- iconButton refresh

    let eventStateChangedHandler :: Handler (DeviceState DHT22)
        eventStateChangedHandler SensorState{..} = case readings of
          Left (_ :: Error) -> putStrLn "[DHT22.eventStateChangedHandler] An error occured when obtaining the readings"
          Right (Temperature t, Humidity h) -> do
            getHandler temperature (pack . show $ t)
            getHandler humidity    (pack . show $ h)

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $
      onElementIDClick (getItemId getTemperatureButton) $ runRemote $
        withResponse GetReadingsRequest
          doNothing
          (\(GetReadingsReply state) -> eventStateChangedHandler state)

    addPageAction $
      onElementIDClick (getItemId getHumidityButton) $ runRemote $
        withResponse GetReadingsRequest
          doNothing
          (\(GetReadingsReply state) -> eventStateChangedHandler state)


    addPageTile $
      let title = unwords [deviceNode deviceAddress, deviceName deviceAddress]
          img   = Nothing
      in mkTile3 title img $ list [ listItem (H.text "Temperature") (H.div $ getItem temperature >> getItem getTemperatureButton)
                                  , listItem (H.text "Humidity")    (H.div $ getItem humidity >> getItem getHumidityButton)
                                  ]

    addPageAction $
      runRemote $
        withResponse GetReadingsRequest
          doNothing
          (\(GetReadingsReply state) -> eventStateChangedHandler state)


  buildSchedule _ = return ()
