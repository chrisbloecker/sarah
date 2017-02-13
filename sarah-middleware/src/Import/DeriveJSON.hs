module Import.DeriveJSON
  ( FromJSON (..), ToJSON (..), Object (..), decode', eitherDecode', object, (.=), (.:)
  , Parser (..), withObject
  , deriveJSON, jsonOptions
  , Text
  , encodeUtf8
  ) where
--------------------------------------------------------------------------------
import Data.Aeson         (FromJSON (..), ToJSON (..), Object (..), decode', eitherDecode', object, (.=), (.:))
import Data.Aeson.Types   (Parser (..), withObject)
import Data.Aeson.TH      (Options (..), SumEncoding (..), deriveJSON, defaultOptions)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
--------------------------------------------------------------------------------

fieldLabel :: String -> String
fieldLabel "sensorReadingDate"   = "date"
fieldLabel "sensorReadingRoom"   = "room"
fieldLabel "sensorReadingSensor" = "sensor"
fieldLabel "sensorReadingValues" = "values"

-- InterfaceDescription
fieldLabel "interfaceName" = "name"
fieldLabel "interfacePort" = "port"
-- DeviceDescription
fieldLabel "deviceName"      = "name"
fieldLabel "deviceModel"     = "model"
fieldLabel "deviceInterface" = "interface"

fieldLabel "temperature" = "temperature"
fieldLabel "fan"         = "fan"
fieldLabel "mode"        = "mode"
fieldLabel "mpower"      = "power"

fieldLabel "unPin"       = "gpio"
fieldLabel "unAddress"   = "i2c"
fieldLabel "unIP"        = "ip"

fieldLabel s             = s

--------------------------------------------------------------------------------

constructorTag :: String -> String
constructorTag = id

--------------------------------------------------------------------------------

jsonOptions :: Options
jsonOptions = Options { fieldLabelModifier      = fieldLabel
                      , constructorTagModifier  = constructorTag
                      , allNullaryToStringTag   = False
                      , omitNothingFields       = True
                      , sumEncoding             = ObjectWithSingleField
                      , unwrapUnaryRecords      = False
                      }
