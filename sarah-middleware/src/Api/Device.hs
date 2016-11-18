{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
--------------------------------------------------------------------------------
module Api.Device
  ( DeviceApi
  , deviceServer
  ) where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8
import Device.AC.Toshiba
import Servant
import Types hiding (Config)
import Raspberry.GPIO
--------------------------------------------------------------------------------

type DeviceApi = "devices" :> Get '[JSON] String

--------------------------------------------------------------------------------

deviceServer :: ServerT DeviceApi AppM
deviceServer = testServer

testServer :: AppM String
testServer = do
  let config = Config { temperature = T20
                      , fan         = FanAuto
                      , mode        = ModeCool
                      , mpower      = Nothing
                      }
  liftIO $ send (Pin 32) (convert config)
  return . unpack $ convert config
