module Sarah.GUI.MiddlewareClient
  where

import Network.WebSockets

middlewareClient :: TVar (HashMap DeviceAddress RemoteEvent) -> ClientApp
middlewareClient listener connection = do
  -- run the client in gui mode so we get all the notification of when something
  -- changes and we can send commands
  sendTextData connection (encodeAsText ModeSubscribe)

  forever $ do
    encoded <- WS.receiveData connection
    case decodeFromText encoded of
      Nothing -> putStrLn "[middlewareClient] Error decoding message: " ++ show encoded
      Just
