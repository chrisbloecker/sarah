{-# LANGUAGE TemplateHaskell   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Settings
  where
--------------------------------------------------------------------------------
import           Import.DeriveJSON
import           Sarah.Middleware.Model (Host, Port, Device)
--------------------------------------------------------------------------------

data Settings = Settings { webPort     :: Port
                         , nodeHost    :: Host
                         , nodePort    :: Port
                         , nodeRole    :: String
                         , backendHost :: Host
                         , backendPort :: Port
                         }

data SlaveSettings = SlaveSettings { masterHost :: Host
                                   , masterPort :: Port
                                   , devices    :: [Device]
                                   }

deriveJSON jsonOptions ''Settings
deriveJSON jsonOptions ''SlaveSettings
