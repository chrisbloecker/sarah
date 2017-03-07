{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote
  where
--------------------------------------------------------------------------------
import Control.Lens                   ((^.))
import Control.Monad                  (void)
import Data.Aeson                     (FromJSON (..), eitherDecode')
import Data.Aeson.Types               (Parser)
import Data.Text.Encoding             (encodeUtf8)
import Graphics.UI.Threepenny  hiding (map)
import Network.HTTP.Client            (Manager)
import Prelude                 hiding (div, span)
import Sarah.Middleware               (Command, DeviceAddress, Query (..), QueryResult, runEIO, mkCommand, runDeviceCommand, getStatus)
import Sarah.Middleware.Device
import Sarah.GUI.Model                (AppEnv, manager, middleware)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.Sensor.DHT22 as DHT22
import qualified Sarah.Middleware.Device.Example      as ExampleDevice
import qualified Data.ByteString.Lazy as BS
--------------------------------------------------------------------------------

-- models which have an instance of IsDevice can be extended with HasRemote.
class IsDevice model => HasRemote model where
  -- For generating a "widget" that can be used as a remote to control a device.
  -- We need some context though:
  --  - The AppEnv, so we know how to talk to the device
  --  - A DeviceAddress, so we know where the device is. Potentially, there can be
  --    many devices of the same kind available, even at the same node.
  renderRemote :: AppEnv -> DeviceAddress -> model -> UI Element


-- construct a command, build a query, and send it
sendCommand :: AppEnv -> DeviceAddress -> Command -> IO (Maybe QueryResult)
sendCommand appEnv deviceAddress command = do
  let query = Query deviceAddress command
  mres <- runEIO $ runDeviceCommand query (appEnv^.manager) (appEnv^.middleware)
  case mres of
    Left  err -> putStrLn ("[sendCommand] " ++ show err) >> return Nothing
    Right res -> return (Just res)

-- just a liftIO for UI
embedUI :: IO a -> b -> UI a
embedUI = const . liftIO


instance HasRemote DHT22 where
  renderRemote appEnv deviceAddress _ = do
    readTemperatureButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-thermometer-full" ]
    readHumidityButton    <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]

    on click readTemperatureButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand DHT22.GetTemperature)
      case mres of
        Nothing  -> putStrLn "[DHT22.readTemperatureButton.click] No response"
        Just res -> putStrLn "[DHT22.readTemperatureButton.click] Got response"

    on click readHumidityButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand DHT22.GetHumidity)
      case mres of
        Nothing  -> putStrLn "[DHT22.readHumidityButton.click] No response"
        Just res -> putStrLn "[DHT22.readHumidityButton.click] Got response"

    div #+ [ p # set class_ "text-center"
               #+ map element [ readTemperatureButton, readHumidityButton ]
           ]

instance HasRemote ExampleDevice where
  renderRemote appEnv deviceAddress _ = do
    getRandomNumberButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-random" ]
    alwaysFailingButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]

    on click getRandomNumberButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetRandomNumber)
      case mres of
        Nothing  -> putStrLn "[ExampleDevice.getRandomNumberButton.click] No response"
        Just res -> putStrLn "[ExampleDevice.getRandomNumberButton.click] Got response"

    on click alwaysFailingButton $ embedUI $ do
      mres <- sendCommand appEnv deviceAddress (mkCommand ExampleDevice.GetRandomNumber)
      case mres of
        Nothing  -> putStrLn "[ExampleDevice.alwaysFailingButton.click] No response"
        Just res -> putStrLn "[ExampleDevice.alwaysFailingButton.click] Got response"

    div #+ [ p # set class_ "text-center"
               #+ map element [getRandomNumberButton, alwaysFailingButton ]
           ]


instance HasRemote HS110 where
  renderRemote appEnv deviceAddress _ = string "renderRemote.HS110"


instance HasRemote ToshibaAC where
  renderRemote appEnv deviceAddress _ = do
    onButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]
    offButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-off" ]
    coolButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-snowflake-o" ]
    dryButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]
    fanButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-cloud" ]
    ecoButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-leaf" ]
    hiButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-fire" ]

    -- ToDo: get the state of the device and modify it, don't just overwrite the state
    on click onButton   $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click offButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeOff  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click coolButton $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click dryButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeDry  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click fanButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeFan  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click ecoButton  $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeAuto (Just AC.PowerEco))  (appEnv^.manager) (appEnv^.middleware)
    on click hiButton   $ const undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool (Just AC.PowerHigh)) (appEnv^.manager) (appEnv^.middleware)

    div #+ [ p # set class_ "text-center"
               #+ map element [ onButton, offButton ]
           , p # set class_ "text-center"
               #+ map element [ coolButton, dryButton, fanButton ]
           , p # set class_ "text-center"
               #+ map element [ ecoButton, hiButton ]
           ]

-- A Remote works similar to a Device: it stores a value that has an instance of Remote.
-- We don't need to require that models have an instance of IsDevice, that's in
-- the class definition for Remote.
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

-- For turning DeviceReps into Remotes. However, this will only work
fromDeviceRep :: DeviceRep -> Either String Remote
fromDeviceRep = eitherDecode' . BS.fromStrict . encodeUtf8 . unDeviceRep
