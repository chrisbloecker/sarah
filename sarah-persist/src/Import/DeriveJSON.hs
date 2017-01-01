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
fieldLabel s = s

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
