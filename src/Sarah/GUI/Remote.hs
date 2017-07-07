{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote
  ( Remote (..), HasRemote (..), fromDeviceRep
  ) where
--------------------------------------------------------------------------------
import Control.Applicative            ((<|>))
import Data.Aeson                     (FromJSON (..), eitherDecode')
import Data.Aeson.Types               (Parser)
import Data.Text.Encoding             (encodeUtf8)
import Network.HTTP.Client            (Manager)
import Sarah.Middleware.Device
import Sarah.GUI.Model                (HasRemote (..))
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
--------------------------------------------------------------------------------
import Sarah.GUI.Remote.AC.Toshiba   ()
import Sarah.GUI.Remote.Example      ()
import Sarah.GUI.Remote.Power.HS110  ()
import Sarah.GUI.Remote.Sensor.DHT22 ()
--------------------------------------------------------------------------------

-- A Remote works similar to a Device: it stores a value that has an instance of Remote.
-- We don't need to require that models have an instance of IsDevice here, that's
-- already in the class definition for Remote.
data Remote = forall model. (HasRemote model)
            => Remote model

-- This is the second end of the evil way of representing devices. We have to
-- enumerate all the devices we want to be able to parse into a Remote. If a
-- parse succeeds, we wrap the device into a Remote.
instance FromJSON Remote where
  parseJSON v = Remote <$> (parseJSON v :: Parser DHT22)
            <|> Remote <$> (parseJSON v :: Parser HS110)
            <|> Remote <$> (parseJSON v :: Parser ToshibaAC)
            <|> Remote <$> (parseJSON v :: Parser ExampleDevice)
            <|> fail ("Can't parse Remote from JSON: " ++ show v)

-- For turning DeviceReps into Remotes. However, this will only work for those
-- devices that we list in the FromJSON instance for Remote above.
fromDeviceRep :: DeviceRep -> Either String Remote
fromDeviceRep = eitherDecode' . BS.fromStrict . encodeUtf8 . unDeviceRep
