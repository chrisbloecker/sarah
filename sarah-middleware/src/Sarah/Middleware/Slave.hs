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
  , DeviceDescription (..)
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

data DeviceDescription = DeviceDescription DeviceName Device deriving (Show)

instance ToJSON DeviceDescription where
  toJSON (DeviceDescription name device) = Object $ toObject device <> HM.fromList [ "name" .= toJSON name ]
    where
      -- ToDo: this should work but is really not nice
      toObject :: ToJSON a => a -> Object
      toObject a = case toJSON a of
        Object o -> o
        _        -> error "Not an object" -- ToDo: something more informative

instance FromJSON DeviceDescription where
  parseJSON = withObject "DeviceDescription" $ \o ->
    DeviceDescription <$> o .: "name"
                      <*> parseJSON (Object o)

data SlaveSettings = SlaveSettings { nodeName      :: Text
                                   , nodeAddress   :: WebAddress
                                   , masterAddress :: WebAddress
                                   , devices       :: [DeviceDescription]
                                   }
  deriving (Show)
deriveJSON jsonOptions ''SlaveSettings

data State = State { _deviceControllers :: Map Text DeviceController
                   }
makeLenses ''State

--------------------------------------------------------------------------------

startPortManager :: Process PortManager
startPortManager = do
  say "[startPortManager]"
  PortManager <$> spawnLocal portManager
    where
      portManager :: Process ()
      portManager = receiveWait [ matchAny $ \m -> do
                                    say $ "[portManager] Received unexpected message: " ++ show m
                                    portManager
                                ]

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  mmaster <- findMaster (host masterAddress) (show . port $ masterAddress) (seconds 1)
  case mmaster of
    Nothing -> do
      say "No master found... Terminating..."
      -- make sure there's enough time to print the message
      liftIO $ threadDelay 100000
    Just master -> do
      portManager <- startPortManager
      deviceControllers <- fmap M.fromList <$> forM devices $ \(DeviceDescription name (Device model)) -> do pid <- startDeviceController model portManager
                                                                                                             return (name, pid)

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName [ (name, toDeviceRep device) | (DeviceDescription name device) <- devices ])
      linkMaster master

      loop $ State deviceControllers

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
