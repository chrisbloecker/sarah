{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Remote
  where
--------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Import.DeriveJSON
import           Sarah.Middleware.Model
import qualified Sarah.Middleware.Device.AC.Toshiba as Toshiba
--------------------------------------------------------------------------------

newtype Toshiba_16NKV_E = Toshiba_16NKV_E Device
deriveJSON jsonOptions ''Toshiba_16NKV_E

instance Remote Toshiba_16NKV_E where
  type DeviceState Toshiba_16NKV_E = Toshiba.Config

  setState (Toshiba_16NKV_E dev) com = undefined
  getState (Toshiba_16NKV_E dev) = undefined

--------------------------------------------------------------------------------

data Toshiba_RAS_M13NKCV = Toshiba_RAS_M13NKCV Device
deriveJSON jsonOptions ''Toshiba_RAS_M13NKCV

instance Remote Toshiba_RAS_M13NKCV where
  type DeviceState Toshiba_RAS_M13NKCV = Toshiba.Config

  getState = undefined
  setState = undefined

--------------------------------------------------------------------------------

data Toshiba_RAS_M16NKCV = Toshiba_RAS_M16NKCV Device
deriveJSON jsonOptions ''Toshiba_RAS_M16NKCV

instance Remote Toshiba_RAS_M16NKCV where
  type DeviceState Toshiba_RAS_M16NKCV = Toshiba.Config

  getState = undefined
  setState = undefined
