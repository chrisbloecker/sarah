module Import.DeriveJSON
  ( FromJSON, decode', eitherDecode'
  , deriveJSON, jsonOptions
  , Text
  , encodeUtf8
  ) where
--------------------------------------------------------------------------------
import Data.Aeson         (FromJSON, decode', eitherDecode')
import Data.Aeson.TH      (Options (..), SumEncoding (..), deriveJSON, defaultOptions)
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
--------------------------------------------------------------------------------

fieldLabel :: String -> String
fieldLabel "sensorReadingDate"   = "date"
fieldLabel "sensorReadingRoom"   = "room"
fieldLabel "sensorReadingSensor" = "sensor"
fieldLabel "sensorReadingValues" = "values"

fieldLabel "temperature" = "temperature"
fieldLabel "fan"         = "fan"
fieldLabel "mode"        = "mode"
fieldLabel "mpower"      = "power"

fieldLabel "_deviceName"      = "name"
fieldLabel "_deviceModel"     = "model"
fieldLabel "_deviceInterface" = "interface"
fieldLabel "unPin"           = "gpio"

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
