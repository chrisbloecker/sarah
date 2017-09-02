{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Applicative                ((<|>))
import Control.Distributed.Process
import Control.Exception                  (IOException, handle)
import Control.Monad                      (join)
import Crypto                             (autokey, unautokey)
import Data.Aeson                         (ToJSON (..), FromJSON (..), encode, decodeStrict')
import Data.Aeson.Types                   (Parser, Value (..), (.=), (.:), typeMismatch, object, withObject)
import Data.ByteString                    (ByteString)
import Data.Char                          (chr, ord)
import Data.Int                           (Int8)
import Data.Text                          (Text, pack, unpack)
import Data.Text.Encoding                 (encodeUtf8, decodeUtf8)
import GHC.Generics                       (Generic)
import Raspberry.IP
import Sarah.Middleware.Model
import Sarah.Middleware.Slave.Messages
import System.Timeout
--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8      as BS            (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS           (unpack)
import qualified Data.HashMap.Strict        as HM            ((!), keys)
import qualified Network.Socket             as Socket hiding (send, recv)
import qualified Network.Socket.ByteString  as Socket        (send, recv)
import qualified Network.Socket.Options     as Socket        (setSocketTimeouts)
--------------------------------------------------------------------------------

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
  deriving (Generic)

instance ToJSON Month where
  toJSON January   = toJSON ( 1 :: Integer)
  toJSON February  = toJSON ( 2 :: Integer)
  toJSON March     = toJSON ( 3 :: Integer)
  toJSON April     = toJSON ( 4 :: Integer)
  toJSON May       = toJSON ( 5 :: Integer)
  toJSON June      = toJSON ( 6 :: Integer)
  toJSON July      = toJSON ( 7 :: Integer)
  toJSON August    = toJSON ( 8 :: Integer)
  toJSON September = toJSON ( 9 :: Integer)
  toJSON October   = toJSON (10 :: Integer)
  toJSON November  = toJSON (11 :: Integer)
  toJSON December  = toJSON (12 :: Integer)

instance FromJSON Month where
  parseJSON (Number  1) = return January
  parseJSON (Number  2) = return February
  parseJSON (Number  3) = return March
  parseJSON (Number  4) = return April
  parseJSON (Number  5) = return May
  parseJSON (Number  6) = return June
  parseJSON (Number  7) = return July
  parseJSON (Number  8) = return August
  parseJSON (Number  9) = return September
  parseJSON (Number 10) = return October
  parseJSON (Number 11) = return November
  parseJSON (Number 12) = return December
  parseJSON invalid    = fail $ "Invalid JSON: " ++ show invalid

type Year = Integer


data HS110Command = SystemCommand SystemCommand
                  | TimeCommand   TimeCommand
                  | EMeterCommand EMeterCommand

instance ToJSON HS110Command where
  toJSON (SystemCommand systemCommand) = object [ "system" .= toJSON systemCommand ]
  toJSON (TimeCommand   timeCommand)   = object [ "time"   .= toJSON timeCommand   ]
  toJSON (EMeterCommand emeterCommand) = object [ "emeter" .= toJSON emeterCommand ]


data SystemCommand = GetSystemInfo
                   | Reboot
                   | TurnOn
                   | TurnOff
                   | NightMode
                   | DayMode
                   | CheckConfig

instance ToJSON SystemCommand where
  toJSON GetSystemInfo = object [ "get_sysinfo"     .= Null ]
  toJSON Reboot        = object [ "reboot"          .= object [ "delay" .= (1 :: Int) ] ]
  toJSON TurnOn        = object [ "set_relay_state" .= object [ "state" .= (1 :: Int) ] ]
  toJSON TurnOff       = object [ "set_relay_state" .= object [ "state" .= (0 :: Int) ] ]
  toJSON NightMode     = object [ "set_led_off"     .= object [ "off"   .= (1 :: Int) ] ]
  toJSON DayMode       = object [ "set_led_off"     .= object [ "off"   .= (0 :: Int) ] ]


data TimeCommand = GetTime
                 | GetTimeZone

instance ToJSON TimeCommand where
  toJSON GetTime     = object [ "get_time"      .= Null ]
  toJSON GetTimeZone = object [ "get_time_zone" .= Null ]


data EMeterCommand = GetCurrentAndVoltageReadings
                   | GetDailyStatistics Year Month
                   | GetMonthlyStatistics Year

instance ToJSON EMeterCommand where
  toJSON GetCurrentAndVoltageReadings      = object [ "get_realtime"  .= object [] ]
  toJSON (GetDailyStatistics   year month) = object [ "get_daystat"   .= object [ "year" .= toJSON year, "month" .= toJSON month ] ]
  toJSON (GetMonthlyStatistics year)       = object [ "get_monthstat" .= object [ "year" .= toJSON year ] ]


data HS110Result = SystemResult SystemResult
                 | TimeResult   TimeResult
                 | EMeterResult EMeterResult

instance FromJSON HS110Result where
  parseJSON = withObject "HS110Result" $ \o -> case HM.keys o of
    ["system"] -> SystemResult <$> o .: "system"
    ["time"]   -> TimeResult   <$> o .: "time"
    ["emeter"] -> EMeterResult <$> o .: "emeter"
    ks         -> fail $ "Unexpected keys in JSON object: " ++ show ks

data SystemResult = SystemInfo { sysErr     :: Integer
                               , swVersion  :: Text
                               , hwVersion  :: Text
                               , type_      :: Text
                               , model      :: Text
                               , mac        :: Text
                               , deviceId   :: Text
                               , hwId       :: Text
                               , fwId       :: Text
                               , oemId      :: Text
                               , alias      :: Text
                               , devName    :: Text
                               , iconHash   :: Text
                               , relayState :: Integer
                               , onTime     :: Integer
                               , activeMode :: ActiveMode
                               , feature    :: Text
                               , updating   :: Integer
                               , rssi       :: Integer
                               , ledOff     :: Integer
                               , latitude   :: Double
                               , longitude  :: Double
                               }
                  | RebootResult   { sysErr     :: Integer }
                  | SetRelayResult { relayState :: Integer }
                  | SetLEDResult   { ledState   :: Bool    }

data ActiveMode = ModeSchedule

instance FromJSON ActiveMode where
  parseJSON = \case
    String "schedule" -> return ModeSchedule
    invalid           -> fail $ "Invalid JSON: " ++ show invalid


instance FromJSON SystemResult where
  parseJSON = withObject "SystemResult" $ \o -> case HM.keys o of
    ["get_sysinfo"] -> do
      sysinfo <- o .: "get_sysinfo"
      SystemInfo <$> sysinfo .: "err_code"
                 <*> sysinfo .: "sw_ver"
                 <*> sysinfo .: "hw_ver"
                 <*> sysinfo .: "type"
                 <*> sysinfo .: "model"
                 <*> sysinfo .: "mac"
                 <*> sysinfo .: "deviceId"
                 <*> sysinfo .: "hwId"
                 <*> sysinfo .: "fwId"
                 <*> sysinfo .: "oemId"
                 <*> sysinfo .: "alias"
                 <*> sysinfo .: "dev_name"
                 <*> sysinfo .: "icon_hash"
                 <*> sysinfo .: "relay_state"
                 <*> sysinfo .: "on_time"
                 <*> sysinfo .: "active_mode"
                 <*> sysinfo .: "feature"
                 <*> sysinfo .: "updating"
                 <*> sysinfo .: "rssi"
                 <*> sysinfo .: "led_off"
                 <*> sysinfo .: "latitude"
                 <*> sysinfo .: "longitude"

    ["reboot"] -> do
      reboot <- o .: "reboot"
      RebootResult <$> reboot .: "err_code"

    ["set_relay_state"] -> do
      relay <- o .: "set_relay_state"
      SetRelayResult <$> relay .: "err_code"

    ["set_led_off"] -> do
      led <- o .: "set_led_off"
      SetLEDResult <$> led .: "err_code"

    ks -> fail $ "[SystemResult] Unexpected keys in JSON object: " ++ show ks

data TimeResult = GetTimeResult { timeErr :: Integer
                                , year    :: Integer
                                , month   :: Integer
                                , mday    :: Integer
                                , wday    :: Integer
                                , hour    :: Integer
                                , min     :: Integer
                                , sec     :: Integer
                                }
                | GetTimeZoneResult { timeErr   :: Integer
                                    , index     :: Integer
                                    , zoneStr   :: Text
                                    , tzStr     :: Text
                                    , dstOffset :: Integer
                                    }

instance FromJSON TimeResult where
  parseJSON = withObject "TimeResult" $ \o -> case HM.keys o of
    ["get_time"] -> do
      time <- o .: "get_time"
      GetTimeResult <$> time .: "err_code"
                    <*> time .: "year"
                    <*> time .: "month"
                    <*> time .: "mday"
                    <*> time .: "wday"
                    <*> time .: "hour"
                    <*> time .: "min"
                    <*> time .: "sec"
    ["get_timezone"] -> do
      timezone <- o .: "get_timezone"
      GetTimeZoneResult <$> timezone .: "err_code"
                        <*> timezone .: "index"
                        <*> timezone .: "zone_str"
                        <*> timezone .: "tz_str"
                        <*> timezone .: "dst_offset"

data EMeterResult = GetRealtimeReadingsResult { emeterErr :: Integer
                                              , current   :: Double
                                              , voltage   :: Double
                                              , power     :: Double
                                              , total     :: Double
                                              }
                  | GetDailyStatisticsResult { emeterErr :: Integer
                                             , dayList   :: [DayStats]
                                             }
                  | GetMonthlyStatisticsResult { emeterErr :: Integer
                                               , monthList :: [MonthStats]
                                               }

instance FromJSON EMeterResult where
  parseJSON = withObject "EMeterResult" $ \o -> case HM.keys o of
    ["get_realtime"] -> do
      realtime <- o .: "get_realtime"
      GetRealtimeReadingsResult <$> realtime .: "err_code"
                                <*> realtime .: "current"
                                <*> realtime .: "voltage"
                                <*> realtime .: "power"
                                <*> realtime .: "total"

    ["get_daystat"] -> do
      daystat <- o .: "get_daystat"
      GetDailyStatisticsResult <$> daystat .: "err_code"
                               <*> daystat .: "day_list"

    ["get_monthstat"] -> do
      monthstat <- o .: "get_monthstat"
      GetMonthlyStatisticsResult <$> monthstat .: "err_code"
                                 <*> monthstat .: "month_list"


data DayStats = DayStats { dayYear   :: Integer
                         , dayMonth  :: Integer
                         , dayDay    :: Integer
                         , dayEnergy :: Double
                         }

instance FromJSON DayStats where
  parseJSON = withObject "DayStats" $ \o -> DayStats <$> o .: "day" <*> o .: "month" <*> o .: "year" <*> o .: "energy"

data MonthStats = MonthStats { monthYear   :: Integer
                             , monthMonth  :: Integer
                             , monthEnergy :: Double
                             }

instance FromJSON MonthStats where
  parseJSON = withObject "MonthStats" $ \o -> MonthStats <$> o .: "year" <*> o .: "month" <*> o .: "energy"

--------------------------------------------------------------------------------

encrypt :: HS110Command -> ByteString
encrypt = BS.pack
        . ("\0\0\0\0" ++)
        . map chr
        . autokey (171 :: Int)
        . map ord
        . LBS.unpack
        . encode

decrypt :: ByteString -> Maybe HS110Result
decrypt = decodeStrict'
        . BS.pack
        . map chr
        . unautokey (171 :: Int)
        . map ord
        . drop 4
        . BS.unpack

sendCommand :: WebAddress -> HS110Command -> IO (Maybe HS110Result)
sendCommand WebAddress{..} command = Socket.withSocketsDo $ handle errorHandler $ do
  addrInfo <- Socket.getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  socket <- Socket.socket (Socket.addrFamily serverAddr) Socket.Stream Socket.defaultProtocol
  -- set send and receive timeouts to 1s
  Socket.setSocketTimeouts socket 1000000 1000000
  Socket.connect socket (Socket.addrAddress serverAddr)
  Socket.send socket (encrypt command)
  reply <- Socket.recv socket 9999
  Socket.close socket
  return . decrypt $ reply

    where
      errorHandler e = do
        putStrLn $ "[HS110.sendCommand] Send failed: " ++ show (e :: IOException)
        return Nothing


newtype HS110 = HS110 WebAddress deriving (Show)

data ControllerEnv = ControllerEnv { slave       :: Slave
                                   , portManager :: PortManager
                                   , webAddress  :: WebAddress
                                   }

instance IsDevice HS110 where
  data DeviceState HS110 = HS110State { isOn :: Bool }
    deriving (Generic, ToJSON, FromJSON)

  data DeviceRequest HS110 = PowerOn
                           | PowerOff
                           | GetStateRequest
                           | GetReadingsRequest
    deriving (Generic, ToJSON, FromJSON)

  data DeviceReply HS110 = GetStateReply (DeviceState HS110)
                         | GetReadingsReply
                         | EmptyReply
    deriving (Generic, ToJSON, FromJSON)

  startDeviceController (HS110 webAddress) slave portManager = do
    say "[HS110.startDeviceController] starting controller for HS110"
    msystemInfo <- liftIO $ sendCommand webAddress (SystemCommand GetSystemInfo)
    let state = case msystemInfo of
                  Just (SystemResult SystemInfo{..}) -> HS110State { isOn = relayState == 1 }
                  _                                  -> HS110State { isOn = False }
    DeviceController <$> spawnLocal (controller state ControllerEnv{..})

      where
        controller :: DeviceState HS110 -> ControllerEnv -> Process ()
        controller state@HS110State{..} env@ControllerEnv{..} =
          receiveWait [ match $ \(FromPid src (query :: Query)) -> case getCommand (queryCommand query) of
                          Left err -> do
                            say $ "[HS110.controller] Can't decode command: " ++ err
                            controller state env

                          Right command -> case command of
                            PowerOn -> do
                              say "[HS110.controller] Switching on"
                              liftIO $ sendCommand webAddress (SystemCommand TurnOn)
                              let state' = state { isOn = True }
                              send src (mkQueryResult EmptyReply)
                              sendStateChanged slave state'
                              controller state' env

                            PowerOff -> do
                              say "[HS110.controller] Switching off"
                              liftIO $ sendCommand webAddress (SystemCommand TurnOff)
                              let state' = state { isOn = False }
                              send src (mkQueryResult EmptyReply)
                              sendStateChanged slave state'
                              controller state' env

                            GetStateRequest -> do
                              say "[HS110.controller] Getting state"
                              send src (mkQueryResult $ GetStateReply state)
                              controller state env

                            GetReadingsRequest -> do
                              say "[HS110.controller] Getting readings"
                              liftIO $ sendCommand webAddress (EMeterCommand GetCurrentAndVoltageReadings)
                              controller state env

                      , matchAny $ \m -> do
                          say $ "[HS110] Received unexpected message" ++ show m
                          controller state env
                      ]

instance ToJSON HS110 where
  toJSON (HS110 webAddress) = object [ "model" .= String "HS110"
                                     , "ip"    .= toJSON webAddress
                                     ]

instance FromJSON HS110 where
  parseJSON = withObject "HS110" $ \o -> do
    model <- o .: "model" :: Parser Text
    case model of
      "HS110" -> HS110 <$> o .: "ip"
      model   -> fail $ "Invalid model identifier: " ++ show model
