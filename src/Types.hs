module Types
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process.Internal.Types (LocalNode, ProcessId)
--------------------------------------------------------------------------------

data Config = Config { masterPid :: ProcessId
                     , localNode :: LocalNode
                     }
