{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-- This module defines an example device and shows what needs to be done in order
-- to implement a new device.
-- The Language extensions DeriveAnyClass and DeriveGeneric are required for deriving JSON instances.
-- We probably need OverloadedString for manually implementing JSON instances.
-- The language extension TypeFamilies is required for the instance of IsDevice.
--------------------------------------------------------------------------------
module Sarah.Middleware.Device.Example
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Data.Aeson                      (ToJSON (..), FromJSON (..), encode)
import Data.Aeson.Types                (Parser, Value (..), (.=), (.:), object, withObject)
import Data.Text                       (Text, unpack)
import GHC.Generics                    (Generic)
import Raspberry.GPIO
import Sarah.Middleware.Slave.Messages
import Sarah.Middleware.Model          (DeviceController (..), IsDevice (..), PortManager (..), Slave (..), FromPid (..), Query (..), getCommand, mkSuccess, mkError, sendWithPid, encodeAsText)
--------------------------------------------------------------------------------

-- Define the device, let's say it uses a GPIO pin.
newtype ExampleDevice = ExampleDevice Pin deriving (Show)

-- Implement an instance for IsDevice
instance IsDevice ExampleDevice where
  -- Define the device state
  data DeviceState ExampleDevice = Normal
                                 | Star
                                 | Heart
    deriving (Generic, ToJSON, FromJSON)

  -- List all the possible commands the device should support
  -- DeviceCommand needs to have instances for ToJSON and FromJSON
  data DeviceCommand ExampleDevice = GetRandomNumber
                                   | SetState (DeviceState ExampleDevice)
                                   | GetState
                                   | AlwaysFailing
    deriving (Generic, ToJSON, FromJSON)

  -- setup the device and start a server that listens for commands
  startDeviceController (ExampleDevice pin) slave portManager = do
    say "[ExampleDevice.startDeviceController]"
    -- start the server and wrap its pid into a DeviceController
    DeviceController <$> spawnLocal (controller Normal slave portManager pin)

      where
        -- the controller listens for requests and replies to them
        controller :: DeviceState ExampleDevice -> Slave -> PortManager -> Pin -> Process ()
        controller state slave portManager pin =
          receiveWait [ match $ \(FromPid src Query{..}) -> case getCommand queryCommand of
                          Left err -> say $ "[ExampleDevice.controller] Can't decode command: " ++ err
                          Right command -> case command of
                            GetRandomNumber -> do
                              say "[Example.controller] Getting random number"
                              send src (mkSuccess $ encode (42 :: Integer))
                              controller state slave portManager pin

                            SetState state' -> do
                              say "[Example.controller] Setting state"
                              sendStateChanged slave state'
                              controller state' slave portManager pin

                            GetState -> do
                              say "[Example.controller] Getting state"
                              send src (mkSuccess $ encode state)
                              controller state slave portManager pin

                            AlwaysFailing -> do
                              say "[Example.controller] This action always fails"
                              send src (mkError "This command always fails")
                              controller state slave portManager pin
                      , matchAny $ \m -> do
                          say $ "[ExampleDevice.controller] Received unexpected message: " ++ show m
                          controller state slave portManager pin
                      ]


instance ToJSON ExampleDevice where
  toJSON (ExampleDevice (Pin pin)) = object [ "model" .= String "ExampleDevice"
                                            , "gpio" .= toJSON pin
                                            ]

-- A FromJSON instance for the device is necessary so we can configure it through a yml file
instance FromJSON ExampleDevice where
  parseJSON = withObject "ExampleDevice" $ \o -> do
    -- parse the field "model" from the JSON object
    model <- o .: "model" :: Parser Text
    case model of
      "ExampleDevice" -> ExampleDevice <$> (Pin <$> o .: "gpio")
      invalid         -> fail $ "Invalid model identifier: " ++ unpack invalid
