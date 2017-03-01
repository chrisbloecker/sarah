{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote
  where
--------------------------------------------------------------------------------
import Control.Lens                   ((^.))
import Data.Aeson                     (FromJSON (..), eitherDecode)
import Data.Aeson.Types               (Parser)
import Data.Text.Encoding             (encodeUtf8)
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (div, span)
import Sarah.Middleware               (runEIO)
import Sarah.Middleware.Device
import Sarah.GUI.Model                (AppEnv, manager, middleware)
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy   as BS
--------------------------------------------------------------------------------

class IsDevice model => HasRemote model where
  renderRemote :: AppEnv -> model -> UI Element

instance HasRemote DHT22 where
  renderRemote appEnv _ = do
    readTemperatureButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-thermometer-full" ]
    readHumidityButton    <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]

    on click readTemperatureButton $ \_ -> undefined
    on click readHumidityButton    $ \_ -> undefined

    div #+ [ p # set class_ "text-center"
               #+ map element [ readTemperatureButton, readHumidityButton ]
           ]

instance HasRemote HS110 where
  renderRemote appEnv _ = string "renderRemote.HS110"

instance HasRemote ToshibaAC where
  renderRemote _ appEnv = do
    onButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]
    offButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-off" ]
    coolButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-snowflake-o" ]
    dryButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]
    fanButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-cloud" ]
    ecoButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-leaf" ]
    hiButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-fire" ]

    -- ToDo: get the state of the device and modify it, don't just overwrite the state
    on click onButton   $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click offButton  $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeOff  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click coolButton $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click dryButton  $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeDry  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click fanButton  $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeFan  Nothing)             (appEnv^.manager) (appEnv^.middleware)
    on click ecoButton  $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeAuto (Just AC.PowerEco))  (appEnv^.manager) (appEnv^.middleware)
    on click hiButton   $ \_ -> undefined -- runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool (Just AC.PowerHigh)) (appEnv^.manager) (appEnv^.middleware)

    div #+ [ p # set class_ "text-center"
               #+ map element [ onButton, offButton ]
           , p # set class_ "text-center"
               #+ map element [ coolButton, dryButton, fanButton ]
           , p # set class_ "text-center"
               #+ map element [ ecoButton, hiButton ]
           ]

data Remote = forall model. HasRemote model => Remote model

instance FromJSON Remote where
  parseJSON v = Remote <$> (parseJSON v :: Parser DHT22)
            <|> Remote <$> (parseJSON v :: Parser HS110)
            <|> Remote <$> (parseJSON v :: Parser ToshibaAC)
            <|> fail ("Can't parse Remote from JSON: " ++ show v)

fromDeviceRep :: DeviceRep -> Either String Remote
fromDeviceRep = eitherDecode . BS.fromStrict . encodeUtf8 . unDeviceRep
