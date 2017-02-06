{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Monad
import           Control.Lens
import           Data.Map
import           Data.Constraint
import           Import.DeriveJSON
import           Sarah.Middleware.Master.Messages
import           Sarah.Middleware.Model           hiding (master, nodeName)
import           Sarah.Middleware.Slave.Messages
import           Sarah.Middleware.Device          hiding (State)
import           Sarah.Middleware.Util
import           Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map as M
--------------------------------------------------------------------------------

data DeviceDescription = DeviceDescription { _deviceName :: Text
                                           , _device     :: Device
                                           }
makeLenses ''DeviceDescription
deriveJSON jsonOptions ''DeviceDescription

data SlaveSettings = SlaveSettings { slaveNode  :: WebAddress
                                   , master     :: WebAddress
                                   , interfaces :: [Interface]
                                   , devices    :: [Device]
                                   , nodeName   :: Text
                                   , room       :: Text
                                   }
deriveJSON jsonOptions ''SlaveSettings

data State = State { _interfaceControllers :: [(Interface, ProcessId)]
                   , _deviceControllers    :: Map Int       ProcessId
                   }
makeLenses ''State

--------------------------------------------------------------------------------

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  mmaster <- findMaster (host master) (show . port $ master) (seconds 1)
  case mmaster of
    Nothing -> do
      say "No master found... Terminating..."
      -- make sure there's enough time to print the message
      liftIO $ threadDelay 100000
    Just master -> do
      interfaceControllers <- zip interfaces <$> forM interfaces startInterfaceController
      deviceControllers    <- fromList . zip [1..] <$> forM devices (\(Device model) -> startDeviceController model)

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName)
      linkMaster master

      loop $ State interfaceControllers deviceControllers

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
