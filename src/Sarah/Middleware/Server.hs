{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Server
  ( ConnectionMode (..)
  , initState
  , server
  )
  where
--------------------------------------------------------------------------------
import Control.Exception
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson              (ToJSON, FromJSON)
import Data.Text               (Text, unpack)
import GHC.Generics            (Generic)
import Network.WebSockets
import Sarah.Middleware.Types  (Query (..), decodeFromText)
--------------------------------------------------------------------------------

data ConnectionMode = ModeSubscribe
                    | ModeCommand
  deriving (Generic, ToJSON, FromJSON)

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


server :: ServerState -> ServerApp
server state@ServerState{..} pending = do
  connection <- acceptRequest pending

  -- start a ping thread to "force" the remote side to stay alive
  forkPingThread connection 30

  newId <- atomically $ do
    newId <- readTVar nextId
    modifyTVar nextId (+1)
    return newId

  -- receive the connection mode that should be used
  encoded <- receiveData connection

  case decodeFromText encoded of
    Nothing -> putStrLn $ "Unexpected message: " ++ show encoded
    Just mode -> case mode of
      ModeSubscribe -> do
        atomically $ modifyTVar subscribers ((newId, connection) :)
        forever $ do
          message <- receiveData connection :: IO Text
          putStrLn $ "Received message: " ++ unpack message
        atomically $ modifyTVar subscribers (filter ((/= newId) . fst))

      -- run the server in command mode, i.e. receive a stream of command messages
      -- and forward them to the master
      -- ToDo: we need a way to reply to a command directly... how?
      ModeCommand -> forever $ do
        encoded <- receiveData connection
        case decodeFromText encoded of
          Nothing -> return ()
          Just query@Query{..} -> return ()
