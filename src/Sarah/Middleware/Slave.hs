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
import Control.Monad                            (forM, void)
import Data.Aeson.Types                         (Value (..))
import Data.List                                ((\\), elem, notElem)
import Data.Map.Strict                          (Map)
import Data.Monoid                              ((<>))
import Data.Text                                (unpack)
import Import.DeriveJSON
import Raspberry.Hardware
import Sarah.Middleware.Device
import Sarah.Middleware.Distributed             (NodeInfo (..), sendWithPid)
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model            hiding (master, nodeName)
import Sarah.Middleware.Model.Interface
import Sarah.Middleware.Slave.Messages
import Sarah.Middleware.Util
import Sarah.Middleware.Types                   (FromPid (..), DeviceName, Query (..), QueryResult (..), deviceName)
import Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map.Strict   as M  (fromList, lookup)
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

data State = State { deviceControllers :: Map DeviceName DeviceController
                   }

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
loop state@State{..} =
  receiveWait [ match $ \(FromPid src query@Query{..}) -> do
                  -- ToDo: should we check if the query was intended for this node?
                  let deviceName' = deviceName queryTarget
                  case M.lookup deviceName' deviceControllers of
                    Nothing                      -> say $ "[slave] Unknown device: " ++ unpack deviceName'
                    Just (DeviceController dest) -> void $ spawnLocal $ do
                      sendWithPid dest query
                      receiveWait [ match    $ \result@QueryResult{..} -> send src result
                                  , matchAny $ \m                      -> say $ "[slave] Unexpected message: " ++ show m
                                  ]
                  loop state

              , match $ \Terminate ->
                  return ()
              ]