{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module Sarah.Persist.Settings
  where
--------------------------------------------------------------------------------
import GHC.Generics (Generic)
import System.Envy
--------------------------------------------------------------------------------

data Settings = Settings { appPort    :: Int
                         , dbHost     :: String
                         , dbPort     :: Int
                         , dbUser     :: String
                         , dbPassword :: String
                         , dbDatabase :: String
                         }
  deriving (Generic, Show)

instance DefConfig Settings where
  defConfig = Settings { appPort    = 8080
                       , dbHost     = "localhost"
                       , dbPort     = 3306
                       , dbUser     = "anonymous"
                       , dbPassword = "secret"
                       , dbDatabase = "test"
                       }

instance FromEnv Settings
