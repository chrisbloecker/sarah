{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module ComplicatedGlobalnet
  ( Backend (newLocalNode)
  , initializeBackend
  ) where
--------------------------------------------------------------------------------
import           Control.Concurrent                       (ThreadId)
import           Control.Concurrent.MVar                  (MVar, newMVar, modifyMVar_)
import           Control.Distributed.Process              (RemoteTable)
import           Control.Exception                        (throw)
import           Control.Exception.Base                   (IOException)
import           Network.Socket                           (HostName, ServiceName)
import           Network.Transport                        (Transport)
import           Network.Transport.TCP                    (createTransport, defaultTCPParameters)
--------------------------------------------------------------------------------
import qualified Control.Distributed.Process.Node as Node (LocalNode, newLocalNode)
--------------------------------------------------------------------------------

data Backend = Backend { newLocalNode :: IO Node.LocalNode }

data BackendState = BackendState { localNodes      :: [Node.LocalNode]
                                 , discoveryDaemon :: ThreadId
                                 }

--------------------------------------------------------------------------------

initializeBackend :: HostName -> ServiceName -> RemoteTable -> IO Backend
initializeBackend host port remoteTable = do
  mtransport <- createTransport host port defaultTCPParameters
  case mtransport of
    Left err -> throw err
    Right transport -> do
      mvar <- newMVar (BackendState [])
      let newLocalNode    = mkNewLocalNode transport remoteTable mvar
          discoveryDaemon = undefined
      return Backend {..}


mkNewLocalNode :: Transport -> RemoteTable -> MVar BackendState -> IO Node.LocalNode
mkNewLocalNode transport remoteTable backendState = do
  localNode <- Node.newLocalNode transport remoteTable
  modifyMVar_ backendState $ \backendState@BackendState {..} ->
    return backendState { localNodes = localNode : localNodes }
  return localNode
