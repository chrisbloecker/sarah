{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Server
  ( ConnectionMode (..)
  , runServer
  , initState
  , subscribers
  )
  where
--------------------------------------------------------------------------------
import Control.Exception
import Control.Concurrent.STM
import Control.Distributed.Process  (Process, expect)
import Control.Monad
import Data.Aeson                   (ToJSON, FromJSON)
import Data.Text                    (Text, unpack)
import GHC.Generics                 (Generic)
import Network.WebSockets    hiding (Request, runServer)
import Sarah.Middleware.Master.Messages
import Sarah.Middleware.Model       (Config (..), Master, unMaster, Query (..), QueryResult, encodeAsText, decodeFromText, mkError, sendWithPid)
--------------------------------------------------------------------------------

data ConnectionMode = ModeSubscribe
                    | ModeCommand
                    | ModeMaster
  deriving (Generic, ToJSON, FromJSON)

instance WebSocketsData ConnectionMode where
  toLazyByteString ModeSubscribe = "ModeSubscribe"
  toLazyByteString ModeCommand   = "ModeCommand"
  toLazyByteString ModeMaster    = "ModeMaster"

  fromLazyByteString "ModeSubscribe" = ModeSubscribe
  fromLazyByteString "ModeCommand"   = ModeCommand
  fromLazyByteString "ModeMaster"    = ModeMaster
  fromLazyByteString unknown         = error $ "Unknown mode: " ++ show unknown


data ServerState = ServerState { subscribers :: TVar [(Integer, Connection)]
                               , nextId      :: TVar Integer
                               }

--------------------------------------------------------------------------------

initState :: IO ServerState
initState = do
  subscribers <- atomically $ newTVar []
  nextId      <- atomically $ newTVar 0
  return ServerState{..}


disconnect :: ServerState -> Integer -> IO ()
disconnect ServerState{..} connectionId = do
  putStrLn $ "[server] Client disconnected: " ++ show connectionId
  atomically $ modifyTVar subscribers (filter ((/= connectionId) . fst))


runServer :: Config -> ServerState -> PendingConnection -> IO ()
runServer Config{..} state@ServerState{..} pending = do
  connection <- acceptRequest pending

  -- start a ping thread to "force" the remote side to stay alive
  forkPingThread connection 30

  newId <- atomically $ do
    newId <- readTVar nextId
    modifyTVar nextId (+1)
    return newId

  -- receive the connection mode that should be used
  connectionMode <- receiveData connection

  case connectionMode of
    ModeSubscribe -> do
      atomically $ modifyTVar subscribers ((newId, connection) :)
      -- Just keep the websocket open, we don't really expect any messages
      forever $ do
        message <- receiveData connection :: IO Text
        putStrLn $ "Received message: " ++ unpack message
      atomically $ modifyTVar subscribers (filter ((/= newId) . fst))

    -- run the server in command mode, i.e. receive exactly one command
    -- and reply with exactly one response
    ModeCommand -> do
      encoded <- receiveData connection
      case decodeFromText encoded of
        Nothing -> do
          putStrLn $ "[server] Error decoding command: " ++ show encoded
          sendTextData connection (encodeAsText $ mkError "Couldn't decode command")

        Just query@Query{..} -> do
          response <- runLocally $ do
                        sendWithPid (unMaster master) query
                        expect :: Process QueryResult
          sendTextData connection (encodeAsText response)

    -- run the server in master mode, i.e. receive exactly one command and
    -- forward it to the master
    ModeMaster -> do
      putStrLn "Running in ModeMaster"
      MasterRequest request <- receiveData connection
      putStrLn "Sending request to master"
      reply <- serverMaster master request
      putStrLn "Sending reply to gui"
      sendBinaryData connection reply

  where
    serverMaster :: (IsMasterCommand command) => Master -> Request command -> IO (Reply command)
    serverMaster master request = runLocally $ sendWithPid (unMaster master) request >> expect
