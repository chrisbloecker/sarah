{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Websocket
  where
--------------------------------------------------------------------------------
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad          (forever)
import Control.Monad.Reader   (ask, liftIO)
import Data.Aeson             (ToJSON, FromJSON, encode)
import Data.HashMap.Strict    (HashMap)
import Data.Text              (Text, unpack)
import Raspberry.IP           (WebAddress (..))
import Sarah.GUI.Model
import Sarah.Middleware
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM
import qualified Network.WebSockets  as WS
--------------------------------------------------------------------------------

subscribeDeviceStateChanges :: TVar (HashMap DeviceAddress RemoteEvent) -> WS.ClientApp ()
subscribeDeviceStateChanges listeners connection = do
  -- run the client in gui mode so we get all the notification of when something
  -- changes and we can send commands
  WS.sendBinaryData connection ModeSubscribe

  forever $ do
    StateChangeEvent deviceAddress encodedState <- WS.receiveData connection
    mhandler <- atomically $ fmap snd . HM.lookup deviceAddress <$> readTVar listeners
    case mhandler of
      Nothing      -> doNothing
      Just handler -> handler encodedState


withResponse :: (IsDevice model, reply ~ DeviceReply model)
             => DeviceRequest model -> ErrorHandler -> SuccessHandler reply -> RemoteRunner ()
withResponse command errorHandler successHandler = do
  RemoteRunnerEnv{..} <- ask
  let query = mkQuery deviceAddress command
  response <- liftIO $ WS.runClient (host middleware) (port middleware) "/" $ \connection -> do
    WS.sendBinaryData connection ModeCommand
    WS.sendTextData connection query
    WS.receiveData connection
  liftIO $ case getQueryResult response of
    Left err -> print err >> errorHandler
    Right reply -> successHandler reply


-- fire and forget
withoutResponse :: (IsDevice model)
                => DeviceRequest model -> RemoteRunner ()
withoutResponse command = do
  RemoteRunnerEnv{..} <- ask
  let query = mkQuery deviceAddress command
  liftIO $ WS.runClient (host middleware) (port middleware) "/" $ \connection -> do
    WS.sendBinaryData connection ModeCommand
    WS.sendTextData connection query


toMaster :: (IsMasterCommand command)
         => WebAddress -> MRequest command -> IO (MReply command)
toMaster middleware request =
  WS.runClient (host middleware) (port middleware) "/" $ \connection -> do
    WS.sendBinaryData connection ModeMaster
    WS.sendBinaryData connection (mkMasterRequest request)
    WS.receiveData connection
