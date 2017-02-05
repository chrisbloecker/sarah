{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Import.DeriveJSON
import           Sarah.Middleware.Master.Messages
import           Sarah.Middleware.Model           hiding (master, nodeName)
import           Sarah.Middleware.Slave.Messages
import           Sarah.Middleware.Device
import           Sarah.Middleware.Remote
import           Sarah.Middleware.Util
import           Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map as M
--------------------------------------------------------------------------------

data DeviceDescription = DeviceDescription { _deviceDescriptionName      :: Text
                                           , _deviceDescriptionInterface :: Interface
                                           , _deviceDescriptionModel     :: DeviceModel
                                           }
makeLenses ''DeviceDescription
deriveJSON jsonOptions ''DeviceDescription

data SlaveSettings = SlaveSettings { _slaveNode :: WebAddress
                                   , _master    :: WebAddress
                                   , _devices   :: [DeviceDescription]
                                   , _nodeName  :: Text
                                   , _room      :: Text
                                   }
makeLenses ''SlaveSettings
deriveJSON jsonOptions ''SlaveSettings

data State = State { _deviceControllers :: Map Int ProcessId }
makeLenses ''State

--------------------------------------------------------------------------------

-- ToDo: this should probably be somewhere else
--mkRemote :: Remote a => DeviceDescription -> a
mkRemote :: DeviceDescription -> Toshiba_16NKV_E
mkRemote desc =
  let device = Device (desc^.deviceDescriptionName) (desc^.deviceDescriptionInterface)
  in case desc^.deviceDescriptionModel of
       Model_Toshiba_16NKV_E     -> Toshiba_16NKV_E device
       Model_Toshiba_RAS_M13NKCV -> undefined
       Model_Toshiba_RAS_M16NKCV -> undefined

runSlave :: SlaveSettings -> Process ()
runSlave settings = do
  mmaster <- findMaster (settings^.master & host) (settings^.master & port & show) (seconds 1)
  case mmaster of
    Nothing -> do
      say "No master found... Terminating..."
      -- make sure there's enough time to print the message
      liftIO $ threadDelay 100000
    Just master -> do
      deviceControllers <- fromList . zip [1..] <$> forM devices $ undefined

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName devices)
      linkMaster master

      loop $ State deviceControllers

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
