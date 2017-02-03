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
import           Sarah.Middleware.Model
import           Sarah.Middleware.Slave.Messages
import           Sarah.Middleware.Device
import           Sarah.Middleware.Util
import           Sarah.Persist.Model
--------------------------------------------------------------------------------
import qualified Data.Map as M
--------------------------------------------------------------------------------

data SlaveSettings = SlaveSettings { slaveNode :: WebAddress
                                   , master    :: WebAddress
                                   , devices   :: [Device]
                                   , nodeName  :: Text
                                   , room      :: Room
                                   }
deriveJSON jsonOptions ''SlaveSettings

data State = State { _deviceProcesses :: Map Int ProcessId }
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
      deviceProcesses <- fromList . zip [1..] <$> mapM (setupDevice nodeName master room) devices

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName devices)
      linkMaster master

      loop $ State deviceProcesses

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
