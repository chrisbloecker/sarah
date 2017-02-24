{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent                       (threadDelay)
import Control.Distributed.Process
import Control.Monad
import Control.Lens                      hiding ((.=))
import Data.Aeson.Types                         (Value (..))
import Data.List                                ((\\), elem, notElem)
import Data.Map                                 (Map, (!), fromList)
import Data.Monoid                              ((<>))
import Import.DeriveJSON
import Raspberry.Hardware
import Sarah.Middleware.Device           hiding (State)
import Sarah.Middleware.Distributed
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model            hiding (master, nodeName)
import Sarah.Middleware.Model.Interface
import Sarah.Middleware.Slave.Messages
import Sarah.Middleware.Util
import Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map          as M  (fromList)
import qualified Data.HashMap.Lazy as HM (fromList)
--------------------------------------------------------------------------------

data SlaveSettings = SlaveSettings { nodeName      :: Text
                                   , nodeAddress   :: WebAddress
                                   , masterAddress :: WebAddress
                                   , devices       :: [(DeviceName, Device)]
                                   }
  deriving (Show)

-- ToDo: this should work but is really not nice
toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "Not an object" -- ToDo: something more informative

instance ToJSON SlaveSettings where
  toJSON SlaveSettings{..} = object [ "nodeName"      .= toJSON nodeName
                                    , "nodeAddress"   .= toJSON nodeAddress
                                    , "masterAddress" .= toJSON masterAddress
                                    , "devices"       .= toJSON (map (uncurry encodeDevice) devices)
                                    ]
    where
      encodeDevice :: DeviceName -> Device -> Value
      encodeDevice name device = Object $ toObject device <> HM.fromList [ "name" .= toJSON name ]

instance FromJSON SlaveSettings where
  parseJSON = withObject "SlaveSettings" $ \o -> do
    nodeName      <- o .: "nodeName"
    nodeAddress   <- o .: "nodeAddress"
    masterAddress <- o .: "masterAddress"
    devices_      <- o .: "devices" >>= \case
                            Array a -> fmap decodeDevice a
                            invalid -> fail $ "Unexpected json: " ++ show invalid
    fail $ show devices_
    devices <- undefined
    return SlaveSettings{..}

    where
      decodeDevice :: Value -> Parser (DeviceName, Device)
      decodeDevice = withObject "Device" $ \o -> (,) <$> o .: "name"
                                                     <*> parseJSON (Object o)

data State = State { _deviceControllers :: Map Text DeviceController
                   }
makeLenses ''State

--------------------------------------------------------------------------------

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  mmaster <- findMaster (host masterAddress) (show . port $ masterAddress) (seconds 1)
  case mmaster of
    Nothing -> do
      say "No master found... Terminating..."
      -- make sure there's enough time to print the message
      liftIO $ threadDelay 100000
    Just master -> do
      portManager <- undefined
      deviceControllers <- undefined -- fmap fromList <$> forM devices $ \(Device model) -> do pid <- startDeviceController model portManager
                                      --                                            return ("SomeDevice", pid)

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName [])
      linkMaster master

      loop $ State deviceControllers

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
