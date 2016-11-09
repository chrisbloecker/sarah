module Util
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process                (Process, WhereIsReply (WhereIsReply), whereisRemoteAsync, receiveTimeout, match)
import Control.Distributed.Process.Internal.Types (NodeId (NodeId))
import Network.Socket                             (HostName, ServiceName)
import Network.Transport.TCP                      (encodeEndPointAddress)
import Types
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

findMaster :: HostName -> ServiceName -> Timeout -> Process (Maybe Master)
findMaster host port timeout = do
  whereisRemoteAsync (NodeId $ encodeEndPointAddress host port 0) "master"
  mpid <- receiveTimeout (unTimeout timeout) [ match $ \(WhereIsReply _ pid) ->
                                                 return pid
                                             ]
  let res = Master <$> _f mpid
  return res
