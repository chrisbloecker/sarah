{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.AC.Toshiba
  where
--------------------------------------------------------------------------------
import Control.Monad                  (unless)
import Control.Monad.Reader           (runReaderT, lift, ask)
import Data.Foldable                  (traverse_)
import Data.Functor.Contravariant     ((>$), contramap)
import Data.Text                      (Text, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (span, unwords)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (EncodedDeviceState, decodeDeviceState, QueryResult (..), mkCommand, DeviceAddress (..))
import Sarah.Middleware.Device        (ToshibaAC)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

instance HasRemote ToshibaAC where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask

    let emptyTemperature = "--"                     :: Text
        emptyFanlevel    = "--"                     :: Text
        grey             = ""                       :: Text
        accented         = "mdl-color-text--accent" :: Text

    powerSwitch  <- lift $ reactiveToggle False
    temperature' <- lift $ reactiveLabel  emptyTemperature
    fanlevel'    <- lift $ reactiveLabel  emptyFanlevel
    mode'        <- lift $ reactiveLabel  "Off"
    powerMode'   <- lift $ reactiveLabel  "Normal"

    tempDownButtonId <- newIdent
    tempUpButtonId   <- newIdent
    fanDownButtonId  <- newIdent
    fanUpButtonId    <- newIdent

    let tempDownButton = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue tempDownButtonId) $
                             icon chevron_left
        tempUpButton   = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue tempUpButtonId) $
                             icon chevron_right
        fanDownButton  = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue fanDownButtonId) $
                             icon chevron_left
        fanUpButton    = H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue fanUpButtonId) $
                             icon chevron_right


    autoButton   <- lift $ reactiveListItem "Auto"   grey
    coolButton   <- lift $ reactiveListItem "Cool"   grey
    dryButton    <- lift $ reactiveListItem "Dry"    grey
    fanButton    <- lift $ reactiveListItem "Fan"    grey
    normalButton <- lift $ reactiveListItem "Normal" grey
    ecoButton    <- lift $ reactiveListItem "Eco"    grey
    hiButton     <- lift $ reactiveListItem "High"   grey

    let eventStateChangedHandler :: Handler (Toshiba.DeviceState ToshibaAC)
        eventStateChangedHandler Toshiba.Config{..} =
          if mode == Toshiba.ModeOff
            then do
              getHandler powerSwitch False
              getHandler temperature' emptyTemperature
              getHandler fanlevel'    emptyFanlevel
              getHandler autoButton   grey
              getHandler coolButton   grey
              getHandler dryButton    grey
              getHandler fanButton    grey
              getHandler ecoButton    grey
              getHandler hiButton     grey

          else do
            getHandler powerSwitch True

            case temperature of
              Temperature t -> getHandler temperature' (pack $ show t ++ "°C")

            getHandler fanlevel' $ case fan of
              Toshiba.FanAuto     -> "Auto"      :: Text
              Toshiba.FanQuiet    -> "Quiet"     :: Text
              Toshiba.FanVeryLow  -> "Very Low"  :: Text
              Toshiba.FanLow      -> "Low"       :: Text
              Toshiba.FanNormal   -> "Normal"    :: Text
              Toshiba.FanHigh     -> "High"      :: Text
              Toshiba.FanVeryHigh -> "Very High" :: Text

            case mode of
              Toshiba.ModeAuto -> getHandler autoButton accented >> getHandler coolButton grey     >> getHandler dryButton grey     >> getHandler fanButton grey     >> getHandler mode' ("Auto" :: Text)
              Toshiba.ModeCool -> getHandler autoButton grey     >> getHandler coolButton accented >> getHandler dryButton grey     >> getHandler fanButton grey     >> getHandler mode' ("Cool" :: Text)
              Toshiba.ModeDry  -> getHandler autoButton grey     >> getHandler coolButton grey     >> getHandler dryButton accented >> getHandler fanButton grey     >> getHandler mode' ("Dry"  :: Text)
              Toshiba.ModeFan  -> getHandler autoButton grey     >> getHandler coolButton grey     >> getHandler dryButton grey     >> getHandler fanButton accented >> getHandler mode' ("Fan"  :: Text)
              Toshiba.ModeOff  -> getHandler autoButton grey     >> getHandler coolButton grey     >> getHandler dryButton grey     >> getHandler fanButton grey     >> getHandler mode' ("Off"  :: Text)

            getHandler ecoButton grey
            getHandler hiButton  grey
            case mpower of
              Nothing                ->                                  getHandler powerMode' ("Normal" :: Text)
              Just Toshiba.PowerEco  -> getHandler ecoButton accented >> getHandler powerMode' ("Eco"    :: Text)
              Just Toshiba.PowerHigh -> getHandler hiButton  accented >> getHandler powerMode' ("High"   :: Text)

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $ onElementIDCheckedChange (getItemId powerSwitch) $ \state -> runRemote $ withoutResponse (Toshiba.Write $ if state then Toshiba.PowerOn else Toshiba.PowerOff)
    addPageAction $ onElementIDClick         tempUpButtonId $     runRemote $ withoutResponse (Toshiba.Write Toshiba.UpTemperature)
    addPageAction $ onElementIDClick         tempDownButtonId $   runRemote $ withoutResponse (Toshiba.Write Toshiba.DownTemperature)
    addPageAction $ onElementIDClick         fanUpButtonId $      runRemote $ withoutResponse (Toshiba.Write Toshiba.UpFan)
    addPageAction $ onElementIDClick         fanDownButtonId $    runRemote $ withoutResponse (Toshiba.Write Toshiba.DownFan)
    addPageAction $ onElementIDClick (getItemId autoButton)   $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeAuto))
    addPageAction $ onElementIDClick (getItemId coolButton)   $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeCool))
    addPageAction $ onElementIDClick (getItemId dryButton)    $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeDry))
    addPageAction $ onElementIDClick (getItemId fanButton)    $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetMode Toshiba.ModeFan))
    addPageAction $ onElementIDClick (getItemId normalButton) $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode Nothing))
    addPageAction $ onElementIDClick (getItemId ecoButton)    $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode $ Just Toshiba.PowerEco))
    addPageAction $ onElementIDClick (getItemId hiButton)     $ runRemote $ withoutResponse (Toshiba.Write (Toshiba.SetPowerMode $ Just Toshiba.PowerHigh))

    dropdownMode <- lift $ dropdown (getItem mode') [ getItem autoButton
                                                    , getItem coolButton
                                                    , getItem dryButton
                                                    , getItem fanButton
                                                    ]
    dropdownPowerMode <- lift $ dropdown (getItem powerMode') [ getItem normalButton
                                                              , getItem ecoButton
                                                              , getItem hiButton
                                                              ]

    addPageTile $
      let title  = unwords [deviceNode deviceAddress, deviceName deviceAddress]
      in mkTile title $ list [ listItem (H.text "Power") (getItem powerSwitch)
                             , listItem (H.text "Temperature") (H.div $ tempDownButton >> getItem temperature' >> tempUpButton)
                             , listItem (H.text "Fan") (H.div $ fanDownButton >> getItem fanlevel' >> fanUpButton)
                             , listItem (H.text "Mode")       $ getItem dropdownMode
                             , listItem (H.text "Power Mode") $ getItem dropdownPowerMode
                             ]

    addPageAction $
      runRemote $
        withResponse (Toshiba.Read Toshiba.GetConfig)
          doNothing
          (\(Toshiba.DeviceState config) -> eventStateChangedHandler config)
