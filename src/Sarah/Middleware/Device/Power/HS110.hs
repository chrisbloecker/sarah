{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Sarah.Middleware.Device.Power.HS110
  where

import Control.Distributed.Process
import Data.Aeson.Types            (Value (..), typeMismatch)
import GHC.Generics                (Generic)
import Import.DeriveJSON
import Raspberry.IP
import Sarah.Middleware.Model

newtype HS110 = HS110 WebAddress deriving (Show)

data ControllerEnv = ControllerEnv { slave       :: Slave
                                   , portManager :: PortManager
                                   , webAddress  :: WebAddress
                                   }

instance IsDevice HS110 where
  type DeviceState HS110 = ()

  data DeviceCommand HS110 = SetPower Power
                           | GetPower
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

data Power = PowerOn | PowerOff deriving (Generic, ToJSON, FromJSON)
