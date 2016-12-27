{-# LANGUAGE TemplateHaskell   #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Settings
  ( MasterSettings (..)
  , SlaveSettings (..)
  ) where
--------------------------------------------------------------------------------
import Import.DeriveJSON
import Sarah.Middleware.Model (WebAddress, Port, Device)
--------------------------------------------------------------------------------

data MasterSettings = MasterSettings { masterNode :: WebAddress
                                     , backend    :: WebAddress
                                     , webPort    :: Port
                                     }

data SlaveSettings = SlaveSettings  { slaveNode :: WebAddress
                                    , master    :: WebAddress
                                    , devices   :: [Device]
                                    }

deriveJSON jsonOptions ''MasterSettings
deriveJSON jsonOptions ''SlaveSettings
