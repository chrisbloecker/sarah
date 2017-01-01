{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
import qualified Data.Map as M
--------------------------------------------------------------------------------

data SlaveSettings = SlaveSettings { slaveNode :: WebAddress
                                   , master    :: WebAddress
                                   , devices   :: [Device]
                                   , nodeName  :: Text
                                   }
  deriving (Show)
deriveJSON jsonOptions ''SlaveSettings

data State = State { _deviceProcesses :: Map Int ProcessId }
makeLenses ''State

--------------------------------------------------------------------------------

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  mmaster <- findMaster (host master) (show . port $ master) (seconds 1)
  case mmaster of
    Nothing ->
      say "No master found... Terminating..."
    Just master -> do
      deviceProcesses <- fromList . zip [1..] <$> mapM (liftIO . setupDevice) devices

      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName devices)
      linkMaster master

      loop $ State deviceProcesses

loop :: State -> Process ()
loop state =
  receiveWait [ match $ \Terminate ->
                  return ()
              ]
