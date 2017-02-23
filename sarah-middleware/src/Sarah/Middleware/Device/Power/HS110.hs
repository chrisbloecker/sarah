{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Sarah.Middleware.Device.Power.HS110
  where

import Control.Distributed.Process
import Data.Aeson.Types            (Value (..), typeMismatch)
import Import.DeriveJSON
import Sarah.Middleware.Model

data HS110 = HS110 deriving (Show)

instance IsDevice HS110 where
  type DeviceState HS110 = ()
  startDeviceController _ portManager = do
    say "[HS110.startDeviceController] starting controller for HS110"
    DeviceController <$> spawnLocal undefined

instance ToJSON HS110 where
  toJSON HS110 = "HS110"

instance FromJSON HS110 where
  parseJSON (String "HS110") = return HS110
  parseJSON invalid          = typeMismatch "HS110" invalid
