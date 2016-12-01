{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Settings
  where
--------------------------------------------------------------------------------
import           GHC.Generics (Generic)
import           Import.DeriveJSON
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
  deriving (Generic)

deriveJSON jsonOptions ''Settings
