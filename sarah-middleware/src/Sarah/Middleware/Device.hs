module Sarah.Middleware.Device
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Lens
import           Raspberry.GPIO
import           Sarah.Middleware.Model
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.AC.Toshiba   as Toshiba
import qualified Sarah.Middleware.Device.Sensor.DHT22 as DHT22
--------------------------------------------------------------------------------

setupDevice :: Device -> IO ProcessId
setupDevice device = do
  let node = undefined
  forkProcess node $ case device^.deviceModel of
    AC     Toshiba_RAS_M13NKCV -> toshibaServer $ device^.deviceInterface
    AC     Toshiba_RAS_M16NKCV -> toshibaServer $ device^.deviceInterface
    AC     Toshiba_16NKV_E     -> toshibaServer $ device^.deviceInterface
    Sensor DHT22               -> dht22Server   $ device^.deviceInterface
    _                          -> error "unknown device" -- ToDo: this is really bad...

toshibaServer :: Interface -> Process ()
toshibaServer interface =
  case interface of
    GPIO pin -> do
      _config <- expect
      liftIO $ Toshiba.send pin _config
      toshibaServer interface

dht22Server :: Interface -> Process ()
dht22Server = undefined
