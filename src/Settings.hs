{-# LANGUAGE DeriveGeneric #-}

module Settings
  where

import GHC.Generics (Generic)
import System.Envy

data Settings = Settings { dbHost     :: String
                         , dbPort     :: Int
                         , dbUser     :: String
                         , dbPassword :: String
                         , dbDatabase :: String
                         }
  deriving (Generic, Show)

instance DefConfig Settings where
  defConfig = Settings { dbHost     = "localhost"
                       , dbPort     = 3306
                       , dbUser     = "anonymous"
                       , dbPassword = "secret"
                       , dbDatabase = "test"
                       }

instance FromEnv Settings
