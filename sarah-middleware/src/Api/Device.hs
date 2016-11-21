{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Api.Device
  ( DeviceApi
  , deviceServer
  ) where
--------------------------------------------------------------------------------
import           Control.Monad.IO.Class            (liftIO)
import           Servant
import           Types                  hiding     (Config)
import           Raspberry.GPIO
--------------------------------------------------------------------------------
import qualified Device.AC.Toshiba      as Toshiba

type DeviceApi = "device" :> "ac"
                          :> ReqBody '[JSON] Toshiba.Config
                          :> Post    '[JSON] Toshiba.Config

--------------------------------------------------------------------------------

deviceServer :: ServerT DeviceApi AppM
deviceServer = acServer

acServer :: Toshiba.Config -> AppM Toshiba.Config
acServer config = liftIO $ Toshiba.send (Pin 23) config >> return config
