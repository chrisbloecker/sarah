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

jsonOptions :: Options
jsonOptions = Options { fieldLabelModifier      = fieldLabel
                      , constructorTagModifier  = id
                      , allNullaryToStringTag   = False
                      , omitNothingFields       = True
                      , sumEncoding             = ObjectWithSingleField
                      , unwrapUnaryRecords      = False
                      }
