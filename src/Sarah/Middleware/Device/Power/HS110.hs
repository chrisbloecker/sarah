{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Data.Aeson                  (ToJSON (..), FromJSON (..))
import Data.Aeson.Types            (Parser, Value (..), (.=), (.:), typeMismatch, object, withObject)
import Data.Text                   (Text)
import GHC.Generics                (Generic)
import Raspberry.IP
import Sarah.Middleware.Model
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

data HS110Command = System SystemCommand
                  | Time   TimeCommand
                  | EMeter EMeterCommand

instance ToJSON HS110Command where
  toJSON (System systemCommand) = object [ "system" .= toJSON systemCommand ]
  toJSON (Time   timeCommand)   = object [ "time"   .= toJSON timeCommand   ]
  toJSON (EMeter emeterCommand) = object [ "emeter" .= toJSON emeterCommand ]


data SystemCommand = GetSystemInfo
                   | Reboot
                   | Reset
                   | TurnOn
                   | TurnOff
                   | NightMode
                   | DayMode
                   | CheckConfig

instance ToJSON SystemCommand where
  toJSON GetSystemInfo = object [ "get_sysinfo"     .= Null ]
  toJSON Reboot        = object [ "reboot"          .= object [ "delay" .= (1 :: Int) ] ]
  toJSON Reset         = object [ "reset"           .= object [ "delay" .= (1 :: Int) ] ]
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

--------------------------------------------------------------------------------

newtype HS110 = HS110 WebAddress deriving (Show)

data ControllerEnv = ControllerEnv { slave       :: Slave
                                   , portManager :: PortManager
                                   , webAddress  :: WebAddress
                                   }

instance IsDevice HS110 where
  data DeviceState HS110 = HS110State
    deriving (Generic, ToJSON, FromJSON)

  data DeviceRequest HS110 = PowerOn
                           | PowerOff
                           | GetReadings
    deriving (Generic, ToJSON, FromJSON)

  data DeviceReply HS110 = Empty
    deriving (Generic, ToJSON, FromJSON)

  startDeviceController (HS110 webAddress) slave portManager = do
    say "[HS110.startDeviceController] starting controller for HS110"
    DeviceController <$> spawnLocal (controller ControllerEnv{..})

      where
        controller :: ControllerEnv -> Process ()
        controller env = receiveWait [ matchAny $ \m -> do
                                         say $ "[HS110] Received unexpected message" ++ show m
                                         controller env
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
