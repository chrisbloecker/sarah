{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Monad
import           Control.Lens                     hiding ((.=))
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

data SlaveSettings = SlaveSettings { nodeName      :: Text
                                   , nodeAddress   :: WebAddress
                                   , masterAddress :: WebAddress
                                   , interfaces    :: [InterfaceDescription]
                                   , devices       :: [DeviceDescription]
                                   }

data InterfaceDescription = InterfaceDescription { interfaceName :: Text
                                                 , interfacePort :: Text --forall interface. IsInterface interface => interface
                                                 }

data DeviceDescription = DeviceDescription { deviceName      :: Text
                                           , deviceModel     :: Text --forall model. IsDevice model => model
                                           , deviceInterface :: Text
                                           }
deriveJSON jsonOptions ''DeviceDescription
deriveJSON jsonOptions ''InterfaceDescription
deriveJSON jsonOptions ''SlaveSettings

data State = State { _interfaceControllers :: [(Interface, ProcessId)]
                   , _deviceControllers    :: Map Int       ProcessId
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
      interfaceControllers <- undefined
      deviceControllers    <- undefined

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName)
      linkMaster master

      loop $ State undefined undefined

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
