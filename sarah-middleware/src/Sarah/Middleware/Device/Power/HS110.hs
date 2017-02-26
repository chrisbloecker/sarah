{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Sarah.Middleware.Device.Power.HS110
  where

import Control.Distributed.Process
import Data.Aeson.Types            (Value (..), typeMismatch)
import Import.DeriveJSON
import Sarah.Middleware.Model

data HS110 = HS110 WebAddress deriving (Show)

instance IsDevice HS110 where
  type DeviceState HS110 = ()

  data DeviceCommand HS110 = SetPower Power
                           | GetPower

  startDeviceController (HS110 webAddress) portManager = do
    say "[HS110.startDeviceController] starting controller for HS110"
    DeviceController <$> spawnLocal (controller portManager webAddress)

      where
        controller :: PortManager -> WebAddress -> Process ()
        controller portManager webAddress = receiveWait [ matchAny $ \m -> do
                                                            say $ "[HS110] Received unexpected message" ++ show m
                                                            controller portManager webAddress
                                                        ]

instance ToJSON HS110 where
  toJSON HS110 = "HS110"

instance FromJSON HS110 where
  parseJSON (String "HS110") = return HS110
  parseJSON invalid          = typeMismatch "HS110" invalid

data Power = PowerOn | PowerOff
