module Sarah.Middleware.Util
  ( findMaster
  , masterName

  , milliseconds
  , seconds
  , minutes
  , hours
  ) where
--------------------------------------------------------------------------------
import Control.Monad                              (join)
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types (NodeId (NodeId))
import Control.Distributed.Process.Extras.Internal.Primitives (whereisRemote)
import Network.Socket                             (HostName, ServiceName)
import Network.Transport.TCP                      (encodeEndPointAddress)
import Sarah.Middleware.Model
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
findMaster host port (Timeout timeout) = do
  -- ToDo: Determine the node number, this won't be always 0
  let remoteNode = NodeId $ encodeEndPointAddress host port 0
  whereisRemoteAsync remoteNode masterName
  mpid <- join <$> receiveTimeout timeout [ match $ \(WhereIsReply _ mpid) -> return mpid ]
  return $ mkMaster <$> mpid

duplicates :: (Eq a) => [a] -> [a]
duplicates []     = []
duplicates (x:xs) | x `elem` xs = x : duplicates xs
                  | otherwise   =     duplicates xs

-- the difference l1 - l2
difference :: (Eq a) => [a] -> [a] -> [a]
difference l1 l2 = [x | x <- l1, x `notElem` l2]
