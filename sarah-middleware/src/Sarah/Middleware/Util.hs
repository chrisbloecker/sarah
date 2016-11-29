module Sarah.Middleware.Util
  where
--------------------------------------------------------------------------------
import Control.Monad                              (join)
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types (NodeId (NodeId))
import Control.Distributed.Process.Extras.Internal.Primitives (whereisRemote)
import Network.Socket                             (HostName, ServiceName)
import Network.Transport.TCP                      (encodeEndPointAddress)
import Sarah.Middleware.Types
--------------------------------------------------------------------------------

newtype Timeout = Timeout { unTimeout :: Int } deriving (Eq, Show)

--------------------------------------------------------------------------------

milliseconds :: Int -> Timeout
milliseconds ms = Timeout (1000 * ms)

seconds :: Int -> Timeout
seconds s = milliseconds (1000 * s)

minutes :: Int -> Timeout
minutes m = seconds (60 * m)

hours :: Int -> Timeout
hours h = minutes (60 * h)

--------------------------------------------------------------------------------

masterName :: String
masterName = "master"

findMaster :: HostName -> ServiceName -> Timeout -> Process (Maybe Master)
findMaster host port timeout = do
  let remoteNode = NodeId $ encodeEndPointAddress host port 1
  whereisRemoteAsync remoteNode masterName
  mpid <- join <$> receiveTimeout (unTimeout timeout) [ match $ \(WhereIsReply _ mpid) ->
                                                          return mpid
                                                      ]
  say $ "Master found at " ++ show mpid
  return $ Master <$> mpid

linkMaster :: Master -> Process ()
linkMaster (Master master) = link master
