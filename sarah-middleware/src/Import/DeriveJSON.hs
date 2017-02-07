module Import.DeriveJSON
  ( FromJSON (..), ToJSON (..), Object (..), decode', eitherDecode', object, (.=), (.:)
  , deriveJSON, jsonOptions
  , Text
  , encodeUtf8
  ) where
--------------------------------------------------------------------------------
import Data.Aeson         (FromJSON (..), ToJSON (..), Object (..), decode', eitherDecode', object, (.=), (.:))
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

fieldLabel "unPin"            = "gpio"

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
