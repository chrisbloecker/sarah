{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Sarah.Persist.Settings
  where
--------------------------------------------------------------------------------
import Import.DeriveJSON
--------------------------------------------------------------------------------

data Settings = Settings { appPort    :: Int
                         , dbHost     :: String
                         , dbPort     :: Int
                         , dbUser     :: String
                         , dbPassword :: String
                         , dbDatabase :: String
                         }
  deriving (Show)

deriveJSON jsonOptions ''Settings
