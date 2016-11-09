{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module Settings
  where
--------------------------------------------------------------------------------
import GHC.Generics (Generic)
import System.Envy
--------------------------------------------------------------------------------

data Settings = Settings { webPort     :: Int
                         , nodeHost    :: String
                         , nodePort    :: String
                         , nodeRole    :: String
                         , masterHost  :: String
                         , masterPort  :: String
                         , backendHost :: String
                         , backendPort :: Int
                         }
  deriving (Generic, Show)

instance DefConfig Settings where
  defConfig = Settings { webPort     = 8080
                       , nodeHost    = "localhost"
                       , nodePort    = "50000"
                       , nodeRole    = "slave"
                       , masterHost  = "localhost"
                       , masterPort  = "50000"
                       , backendHost = "localhost"
                       , backendPort = 8081
                       }

instance FromEnv Settings
