{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Slave
  ( SlaveSettings (..)
  , runSlave
  ) where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Import.DeriveJSON
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model
import Sarah.Middleware.Slave.Messages  (Terminate (Terminate))
import Sarah.Middleware.Util
--------------------------------------------------------------------------------

data SlaveSettings = SlaveSettings  { slaveNode :: WebAddress
                                    , master    :: WebAddress
                                    , devices   :: [Device]
                                    , nodeName  :: Text
                                    }
deriveJSON jsonOptions ''SlaveSettings

--------------------------------------------------------------------------------

runSlave :: SlaveSettings -> Process ()
runSlave SlaveSettings{..} = do
  mmaster  <- findMaster (host master) (show . port $ master) (seconds 1)
  case mmaster of
    Nothing ->
      say "No master found... Terminating..."
    Just master -> do
      self <- getSelfPid
      nodeUp master self (NodeInfo nodeName devices)
      linkMaster master
      Terminate <- expect
      return ()
