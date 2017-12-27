{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.AC.Toshiba
  where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Reader           (lift, ask)
import Data.Foldable                  (traverse_)
import Data.Text                      (Text, append, pack, unwords)
import Graphics.UI.Material
import Graphics.UI.Threepenny         (Handler, register)
import Prelude                 hiding (span, unwords)
import Physics
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Websocket            (withResponse, withoutResponse)
import Sarah.Middleware               (decodeDeviceState, DeviceAddress (..))
import Sarah.Middleware.Device.AC.Toshiba
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
--------------------------------------------------------------------------------

instance HasSelection Fan where
  toSelectionLabel FanAuto     = "Auto"
  toSelectionLabel FanQuiet    = "Quiet"
  toSelectionLabel FanVeryLow  = "Very Low"
  toSelectionLabel FanLow      = "Low"
  toSelectionLabel FanNormal   = "Normal"
  toSelectionLabel FanHigh     = "High"
  toSelectionLabel FanVeryHigh = "Very High"

  fromSelectionLabel "Auto"      = Right FanAuto
  fromSelectionLabel "Quiet"     = Right FanQuiet
  fromSelectionLabel "Very Low"  = Right FanVeryLow
  fromSelectionLabel "Low"       = Right FanLow
  fromSelectionLabel "Normal"    = Right FanNormal
  fromSelectionLabel "High"      = Right FanHigh
  fromSelectionLabel "Very High" = Right FanVeryHigh
  fromSelectionLabel t           = Left $ "Unexpected selection label for Fan: " `append` t


instance HasSelection Mode where
  toSelectionLabel ModeAuto = "Auto"
  toSelectionLabel ModeCool = "Cool"
  toSelectionLabel ModeDry  = "Dry"
  toSelectionLabel ModeFan  = "Fan"
  toSelectionLabel ModeOff  = "Off"

  fromSelectionLabel "Auto" = Right ModeAuto
  fromSelectionLabel "Cool" = Right ModeCool
  fromSelectionLabel "Dry"  = Right ModeDry
  fromSelectionLabel "Fan"  = Right ModeFan
  fromSelectionLabel "Off"  = Right ModeOff
  fromSelectionLabel t      = Left $ "Unexpected selection label for Mode: " `append` t


instance HasSelection Power where
  toSelectionLabel PowerNormal = "Normal"
  toSelectionLabel PowerHigh   = "High"
  toSelectionLabel PowerEco    = "Eco"

  fromSelectionLabel "Nomrla" = Right PowerNormal
  fromSelectionLabel "High"   = Right PowerHigh
  fromSelectionLabel "Eco"    = Right PowerEco
  fromSelectionLabel t        = Left $ "Unexpected selection label for Power: " `append` t


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

    tempDownButton <- iconButton chevron_left
    tempUpButton   <- iconButton chevron_right
    fanDownButton  <- iconButton chevron_left
    fanUpButton    <- iconButton chevron_right
    autoButton     <- lift $ reactiveListItem "Auto"   grey
    coolButton     <- lift $ reactiveListItem "Cool"   grey
    dryButton      <- lift $ reactiveListItem "Dry"    grey
    fanButton      <- lift $ reactiveListItem "Fan"    grey
    normalButton   <- lift $ reactiveListItem "Normal" grey
    ecoButton      <- lift $ reactiveListItem "Eco"    grey
    hiButton       <- lift $ reactiveListItem "High"   grey

    let eventStateChangedHandler :: Handler (DeviceState ToshibaAC)
        eventStateChangedHandler Config{..} =
          if mode == ModeOff
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
              FanAuto     -> "Auto"      :: Text
              FanQuiet    -> "Quiet"     :: Text
              FanVeryLow  -> "Very Low"  :: Text
              FanLow      -> "Low"       :: Text
              FanNormal   -> "Normal"    :: Text
              FanHigh     -> "High"      :: Text
              FanVeryHigh -> "Very High" :: Text

            case mode of
              ModeAuto -> getHandler autoButton accented >> getHandler coolButton grey     >> getHandler dryButton grey     >> getHandler fanButton grey     >> getHandler mode' ("Auto" :: Text)
              ModeCool -> getHandler autoButton grey     >> getHandler coolButton accented >> getHandler dryButton grey     >> getHandler fanButton grey     >> getHandler mode' ("Cool" :: Text)
              ModeDry  -> getHandler autoButton grey     >> getHandler coolButton grey     >> getHandler dryButton accented >> getHandler fanButton grey     >> getHandler mode' ("Dry"  :: Text)
              ModeFan  -> getHandler autoButton grey     >> getHandler coolButton grey     >> getHandler dryButton grey     >> getHandler fanButton accented >> getHandler mode' ("Fan"  :: Text)
              ModeOff  -> getHandler autoButton grey     >> getHandler coolButton grey     >> getHandler dryButton grey     >> getHandler fanButton grey     >> getHandler mode' ("Off"  :: Text)

            getHandler ecoButton grey
            getHandler hiButton  grey
            case power of
              PowerNormal ->                                  getHandler powerMode' ("Normal" :: Text)
              PowerEco    -> getHandler ecoButton accented >> getHandler powerMode' ("Eco"    :: Text)
              PowerHigh   -> getHandler hiButton  accented >> getHandler powerMode' ("High"   :: Text)

    unregister <- liftIO $ register (decodeDeviceState <$> eventStateChanged) (traverse_ eventStateChangedHandler)

    addPageAction $ onElementIDCheckedChange (getItemId powerSwitch) $ \state -> runRemote $ withoutResponse (Write $ if state then PowerOn else PowerOff)
    addPageAction $ onElementIDClick (getItemId tempUpButton)   $ runRemote $ withoutResponse (Write UpTemperature)
    addPageAction $ onElementIDClick (getItemId tempDownButton) $ runRemote $ withoutResponse (Write DownTemperature)
    addPageAction $ onElementIDClick (getItemId fanUpButton)    $ runRemote $ withoutResponse (Write UpFan)
    addPageAction $ onElementIDClick (getItemId fanDownButton)  $ runRemote $ withoutResponse (Write DownFan)
    addPageAction $ onElementIDClick (getItemId autoButton)     $ runRemote $ withoutResponse (Write (SetMode ModeAuto))
    addPageAction $ onElementIDClick (getItemId coolButton)     $ runRemote $ withoutResponse (Write (SetMode ModeCool))
    addPageAction $ onElementIDClick (getItemId dryButton)      $ runRemote $ withoutResponse (Write (SetMode ModeDry))
    addPageAction $ onElementIDClick (getItemId fanButton)      $ runRemote $ withoutResponse (Write (SetMode ModeFan))
    addPageAction $ onElementIDClick (getItemId normalButton)   $ runRemote $ withoutResponse (Write (SetPowerMode PowerNormal))
    addPageAction $ onElementIDClick (getItemId ecoButton)      $ runRemote $ withoutResponse (Write (SetPowerMode PowerEco))
    addPageAction $ onElementIDClick (getItemId hiButton)       $ runRemote $ withoutResponse (Write (SetPowerMode PowerHigh))

    dropdownMode <- dropdown (getItem mode') [ getItem autoButton
                                             , getItem coolButton
                                             , getItem dryButton
                                             , getItem fanButton
                                             ]
    dropdownPowerMode <- lift $ dropdown (getItem powerMode') [ getItem normalButton
                                                              , getItem ecoButton
                                                              , getItem hiButton
                                                              ]

    addPageTile $
      let title = unwords [deviceNode deviceAddress, deviceName deviceAddress]
          img   = Nothing -- Just "static/img/remote/ac.png"
      in mkTile3 title img $ list [ listItem (H.text "Power")       $ getItem powerSwitch
                                  , listItem (H.text "Temperature") $ H.div $ getItem tempDownButton >> getItem temperature' >> getItem tempUpButton
                                  , listItem (H.text "Fan")         $ H.div $ getItem fanDownButton >> getItem fanlevel' >> getItem fanUpButton
                                  , listItem (H.text "Mode")        $ getItem dropdownMode
                                  , listItem (H.text "Power Mode")  $ getItem dropdownPowerMode
                                  ]

    addPageAction $
      runRemote $
        withResponse (Read GetConfig)
          doNothing
          (\(DeviceState config) -> eventStateChangedHandler config)


  buildSchedule _ = return ()
