module Sarah.GUI.MiddlewareClient
  where

import Network.WebSockets

client :: ClientApp
client connection = do
  -- run the client in gui mode so we get all the notification of when something
  -- changes and we can send commands
  sendTextData connection (encodeAsText ModeGUI)
  forever $ do
    encoded <- WS.receiveData connection
    case decodeT
