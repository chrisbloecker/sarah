{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , InterfaceDescription (..)
  , DeviceDescription (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process
import           Control.Monad
import           Control.Lens                      hiding ((.=))
import           Data.List                                ((\\), elem, notElem)
import           Data.Map                                 (Map, fromList)
import           Import.DeriveJSON
import           Raspberry.Hardware
import           Sarah.Middleware.Device           hiding (State)
import           Sarah.Middleware.Master.Messages
import           Sarah.Middleware.Model            hiding (master, nodeName)
import           Sarah.Middleware.Model.Interface
import           Sarah.Middleware.Slave.Messages
import           Sarah.Middleware.Util
import           Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map as M (fromList)
--------------------------------------------------------------------------------

data InterfaceDescription = InterfaceDescription { interfaceName :: Text
                                                 , interfacePort :: Interface
                                                 }
  deriving (Show)
deriveJSON jsonOptions ''InterfaceDescription

data DeviceDescription = DeviceDescription { deviceName      :: Text
                                           , deviceModel     :: Device
                                           , deviceInterface :: Text
                                           }
  deriving (Show)
deriveJSON jsonOptions ''DeviceDescription

data SlaveSettings = SlaveSettings { nodeName      :: Text
                                   , nodeAddress   :: WebAddress
                                   , masterAddress :: WebAddress
                                   , interfaces    :: [InterfaceDescription]
                                   , devices       :: [DeviceDescription]
                                   }
  deriving (Show)
instance ToJSON SlaveSettings where
  toJSON SlaveSettings{..} = object [ "nodeNmae"      .= toJSON nodeName
                                    , "nodeAddress"   .= toJSON nodeAddress
                                    , "masterAddress" .= toJSON masterAddress
                                    , "interfaces"    .= toJSON interfaces
                                    , "devices"       .= toJSON devices
                                    ]
-- We're making sure in the parsing that all references interfaces are defined
-- and that their names are unique. Similarly, the names of devices must be unique
instance FromJSON SlaveSettings where
  parseJSON = withObject "SlaveSettings" $ \o -> do
    nodeName      <- o .: "nodeName"
    nodeAddress   <- o .: "nodeAddress"
    masterAddress <- o .: "masterAddress"

    interfaces    <- o .: "interfaces"
    let interfaceNames = map interfaceName interfaces
    let duplicateInterfaceNames = duplicates interfaceNames
    unless (null duplicateInterfaceNames) $ fail ("Found duplicate interface names in interfaces: " ++ show duplicateInterfaceNames)

    devices       <- o .: "devices"
    let deviceNames = map deviceName devices
    let duplicateDeviceNames = duplicates deviceNames
    unless (null . duplicates $ deviceNames) $ fail ("Found duplicate device names in devices: " ++ show duplicateDeviceNames)
    let undefinedInterfaces = map deviceInterface devices `difference` interfaceNames
    unless (null undefinedInterfaces) $ fail ("Found undefined interfaces in devices: " ++ show undefinedInterfaces)

    return SlaveSettings{..}

    where
      duplicates :: (Eq a) => [a] -> [a]
      duplicates []     = []
      duplicates (x:xs) | x `elem` xs = x : duplicates xs
                        | otherwise   =     duplicates xs

      -- the difference l1 - l2
      difference :: (Eq a) => [a] -> [a] -> [a]
      difference l1 l2 = [x | x <- l1, x `notElem` l2]

data State = State { _interfaceControllers :: Map Text ProcessId
                   , _deviceControllers    :: Map Text ProcessId
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
      interfaceControllers <- fmap fromList <$> forM interfaces $ \InterfaceDescription{..} -> do pid <- startInterfaceController interfacePort
                                                                                                  return (interfaceName, pid)
      deviceControllers    <- undefined

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName [])
      linkMaster master

      loop $ State interfaceControllers undefined

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
