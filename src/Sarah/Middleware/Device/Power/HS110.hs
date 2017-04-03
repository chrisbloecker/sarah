{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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
import qualified Data.HashMap.Strict        as HM            (keys)
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
  deriving (Generic, ToJSON)

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
  parseJSON = withObject "HS110Result" $ \o -> SystemResult <$> o .: "system"
                                           <|> TimeResult   <$> o .: "time"
                                           <|> EMeterResult <$> o .: "emeter"

data SystemResult = SystemInfo     { errCode    :: ErrorCode
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
                                   , relayState :: Bool
                                   , onTime     :: Integer
                                   , activeMode :: ActiveMode
                                   , feature    :: Text
                                   , updating   :: Bool
                                   , rssi       :: Integer
                                   , ledOff     :: Bool
                                   , latitude   :: Double
                                   , longitude  :: Double
                                   }
                  | RebootResult   { errCode    :: ErrorCode }
                  | SetRelayResult { relayState :: Bool      }
                  | SetLEDResult   { ledState   :: Bool      }

type ErrorCode = Integer
data ActiveMode = Schedule

instance FromJSON SystemResult where
  parseJSON = withObject "SystemResult" $ \o -> case HM.keys o of
    ["get_sysinfo"] -> do
      sysinfo <- o .: "get_sysinfo"
      SystemInfo <$> sysinfo .: "err_code"
                 <*> sysinfo .: "sw_version"
                                  <*> o .: "get_sysinfo" .: "hw_version"
                                  <*> o .: "get_sysinfo" .: "type"
                                  <*> o .: "get_sysinfo" .: "model"
                                  <*> o .: "get_sysinfo" .: "mac"
                                  <*> o .: "get_sysinfo" .: "deviceId"
                                  <*> o .: "get_sysinfo" .: "hwId"
                                  <*> o .: "get_sysinfo" .: "fwId"
                                  <*> o .: "get_sysinfo" .: "oemId"
                                  <*> o .: "get_sysinfo" .: "alias"
                                  <*> o .: "get_sysinfo" .: "dev_name"
                                  <*> o .: "get_sysinfo" .: "icon_hash"
                                  <*> o .: "get_sysinfo" .: "relay_state"
                                  <*> o .: "get_sysinfo" .: "on_time"
                                  <*> o .: "get_sysinfo" .: "active_mode"
                                  <*> o .: "get_sysinfo" .: "feature"
                                  <*> o .: "get_sysinfo" .: "updating"
                                  <*> o .: "get_sysinfo" .: "rssi"
                                  <*> o .: "get_sysinfo" .: "led_off"
                                  <*> o .: "get_sysinfo" .: "latitude"
                                  <*> o .: "get_sysinfo" .: "longitude"

  {-
                                            <|> RebootResult   <$> o .: "reboot"
                                            <|> SetRelayResult <$> o .: "set_relay_state"
                                            <|> SetLEDResult   <$> o .: "set_led_off"-}

data TimeResult = GetTimeResult
                | GetTimeZoneResult

instance FromJSON TimeResult where
  parseJSON = undefined

data EMeterResult = GetCurrentAndVoltageReadingsResult
                  | GetDailyStatisticsResult
                  | GetMonthlyStatisticsResult

instance FromJSON EMeterResult where
  parseJSON = undefined

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
  reply <- Socket.recv socket 2048
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
  data DeviceState HS110 = HS110State { power :: Bool }
    deriving (Generic, ToJSON, FromJSON)

  data DeviceRequest HS110 = PowerOn
                           | PowerOff
                           | GetStateRequest
                           | GetReadingsRequest
    deriving (Generic, ToJSON, FromJSON)

  data DeviceReply HS110 = GetStateReply (DeviceState HS110)
                         | GetReadingsReply
    deriving (Generic, ToJSON, FromJSON)

  startDeviceController (HS110 webAddress) slave portManager = do
    say "[HS110.startDeviceController] starting controller for HS110"
    DeviceController <$> spawnLocal (controller HS110State { power = False } ControllerEnv{..})

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
                              let state' = state { power = True }
                              sendStateChanged slave state'
                              controller state' env

                            PowerOff -> do
                              say "[HS110.controller] Switching off"
                              liftIO $ sendCommand webAddress (SystemCommand TurnOff)
                              let state' = state { power = False }
                              sendStateChanged slave state'
                              controller state' env

                            GetStateRequest -> do
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
