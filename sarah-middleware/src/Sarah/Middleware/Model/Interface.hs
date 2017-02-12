{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
--------------------------------------------------------------------------------
module Sarah.Middleware.Model.Interface
  where
--------------------------------------------------------------------------------
import Control.Applicative
import Control.Distributed.Process
import Import.DeriveJSON
import Raspberry.Hardware
--------------------------------------------------------------------------------

class IsInterface interface where
  startInterfaceController :: interface -> Process ProcessId

newtype GPIO = GPIO Pin deriving (Show)
instance IsInterface GPIO where
  startInterfaceController (GPIO pin) = do
    say "[GPIO.startController]"
    spawnLocal expect
deriveJSON jsonOptions ''GPIO

newtype I2C = I2C Address deriving (Show)
instance IsInterface I2C where
  startInterfaceController (I2C address)= do
    say "[I2C.startController]"
    spawnLocal expect
deriveJSON jsonOptions ''I2C

newtype IP = IP WebAddress deriving (Show)
instance IsInterface IP where
  startInterfaceController (IP webAddress)= do
    say "IP.startController"
    spawnLocal expect
deriveJSON jsonOptions ''IP

data Interface = forall interface. (IsInterface interface, Show interface, FromJSON interface, ToJSON interface) => Interface interface

instance IsInterface Interface where
  startInterfaceController (Interface interface) = startInterfaceController interface

instance Show Interface where
  show (Interface interface) = "Interface " ++ show interface
instance ToJSON Interface where
  toJSON (Interface interface) = toJSON interface
instance FromJSON Interface where
  parseJSON v = Interface <$> (parseJSON v :: Parser GPIO)
            <|> Interface <$> (parseJSON v :: Parser I2C)
            <|> Interface <$> (parseJSON v :: Parser IP)
            <|> fail ("Can't parse Interface from JSON: " ++ show v)
