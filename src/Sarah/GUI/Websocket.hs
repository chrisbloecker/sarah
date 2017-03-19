{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Sarah.GUI.Websocket
  where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad          (forever)
import Control.Monad.Reader   (ask, liftIO)
import Data.Aeson             (ToJSON, FromJSON)
import Data.HashMap.Strict    (HashMap)
import Data.Text              (Text, unpack)
import Raspberry.IP           (WebAddress (..))
import Sarah.GUI.Model
import Sarah.Middleware
import qualified Data.HashMap.Strict as HM
import qualified Network.WebSockets  as WS

subscribeDeviceStateChanges :: TVar (HashMap DeviceAddress RemoteEvent) -> WS.ClientApp ()
subscribeDeviceStateChanges listeners connection = do
  -- run the client in gui mode so we get all the notification of when something
  -- changes and we can send commands
  WS.sendBinaryData connection ModeSubscribe

  forever $ do
    encoded <- WS.receiveData connection
    case decodeFromText encoded :: Maybe (DeviceAddress, EncodedJSON) of
      Nothing -> putStrLn $ "[subscribeDeviceStateChanges] Error decoding message: " ++ show encoded
      Just (deviceAddress, encodedState) -> do
        putStrLn $ "[subscribeDeviceStateChanges] The state of a device has changed: " ++ unpack (deviceNode deviceAddress) ++ ":" ++ unpack (deviceName deviceAddress)
        mhandler <- atomically $ fmap snd . HM.lookup deviceAddress <$> readTVar listeners
        case mhandler of
          Nothing -> putStrLn "[subscribeDeviceStateChanges] No handler registered"
          Just handler -> do
            putStrLn "[subscribeDeviceStateChanges] Updating display"
            handler (getEncoded encodedState)

--
sendCommand :: WebAddress -> DeviceAddress -> Command -> IO (Maybe QueryResult)
sendCommand (WebAddress host port) deviceAddress command = do
  let query = Query deviceAddress command
  WS.runClient host port "/" $ \connection -> do
    -- initialise command mode
    WS.sendBinaryData connection ModeCommand
    WS.sendTextData connection (encodeAsText query)
    decodeFromText <$> WS.receiveData connection


withResponse :: (IsDevice model, ToJSON reply, FromJSON reply)
             => DeviceCommand model -> ErrorHandler -> SuccessHandler reply -> RemoteRunner ()
withResponse command errorHandler successHandler = do
  RemoteRunnerEnv{..} <- ask
  let query = Query deviceAddress (mkCommand command)
  mresponse <- liftIO $ WS.runClient (host middleware) (port middleware) "/" $ \connection -> do
    WS.sendBinaryData connection ModeCommand
    WS.sendTextData connection (encodeAsText query)
    decodeFromText <$> WS.receiveData connection
  liftIO $ case mresponse of
    Nothing -> return ()
    Just (QueryResult result) -> case result of
      Error message -> errorHandler
      Success encoded -> case decodeWrapped encoded of
        Nothing -> putStrLn $ "Error decoding result: " ++ show result
        Just decoded -> successHandler decoded


-- fire and forget
withoutResponse :: (IsDevice model)
                => DeviceCommand model -> RemoteRunner ()
withoutResponse command = do
  RemoteRunnerEnv{..} <- ask
  let query = Query deviceAddress (mkCommand command)
  liftIO $ WS.runClient (host middleware) (port middleware) "/" $ \connection -> do
    WS.sendBinaryData connection ModeCommand
    WS.sendTextData connection (encodeAsText query)


toMaster :: (IsMasterCommand command) => WebAddress -> Request command -> IO (Reply command)
toMaster middleware request = do
  putStrLn "[toMaster] Opening connection"
  WS.runClient (host middleware) (port middleware) "/" $ \connection -> do
    putStrLn "[toMaster] Establishing connection in mode ModeMaster"
    WS.sendBinaryData connection ModeMaster
    putStrLn "[toMaster] Sending request"
    WS.sendBinaryData connection request
    putStrLn "[toMaster] Receiving result"
    WS.receiveData connection
