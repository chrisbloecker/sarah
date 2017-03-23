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
import Data.Aeson.Types            (Parser, Value (..), typeMismatch)
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

type Year = Integer

data HS110Command = System SystemCommand
                  | Time   TimeCommand
                  | EMeter EMeterCommand
  deriving (ToJSON)

data SystemCommand = GetSystemInfo
                   | Reboot
                   | Reset
                   | TurnOn
                   | TurnOff
                   | NightMode
                   | CheckConfig

data TimeCommand = GetTime
                 | GetTimeZone

data EMeterCommand = GetCurrentAndVoltageReadings
                   | GetVGainAndIGain
                   | SetVGainAndIGain
                   | StartCalibration
                   | GetDailyStatistics Year Month
                   | GetMonthlyStatistics Year

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
