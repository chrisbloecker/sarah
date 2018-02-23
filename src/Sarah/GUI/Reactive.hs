{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Reactive
  where
--------------------------------------------------------------------------------
import Control.Monad               (void)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.Aeson                  (ToJSON, FromJSON, encode, decode')
import Data.ByteString.Lazy        (toStrict, fromStrict)
import Data.Text                   (Text, append, pack, unpack)
import Data.Text.Encoding          (encodeUtf8, decodeUtf8)
import Data.UUID                   (toString)
import Data.UUID.V4                (nextRandom)
import Foreign.JavaScript          (FromJS)
import Graphics.UI.Threepenny      (UI, runUI, askWindow)
import Graphics.UI.Threepenny.Core (callFunction, runFunction, ffi, ffiExport)
--------------------------------------------------------------------------------

class HasSelection option where
  toSelectionLabel   :: option -> Text
  fromSelectionLabel :: Text -> Either Text option

unexpectedSelectionLabel :: HasSelection option => Text -> Either Text option
unexpectedSelectionLabel t = Left $ "Unexpected selection label: " `append` t

newIdent :: MonadIO m => m String
newIdent = toString <$> liftIO nextRandom

toBool :: String -> Bool
toBool "true" = True
toBool _      = False

getValue :: FromJS a => String -> UI a
getValue elementId = callFunction $ ffi "$(%1).val()" ('#':elementId)

-- taken from hue-dashboard by blitzcode
onElementIDClick :: String -> UI () -> UI ()
onElementIDClick elementId handler = do
  window   <- askWindow
  exported <- ffiExport $ void $ runUI window handler
  runFunction $ ffi "$(%1).on('click', %2);" ('#':elementId) exported


-- event that occurs when the user changes the checked state of a checkbox
onElementIDCheckedChange :: String -> (Bool -> UI ()) -> UI ()
onElementIDCheckedChange elementId handler = do
  window   <- askWindow
  exported <- ffiExport $ \s -> void $ runUI window (handler . toBool $ s)
  runFunction $ ffi "$(%1).on('change', function(e) { var checked = $(%1).prop('checked').toString(); %2(checked) })" ('#':elementId) exported


-- event that occurs when the user changes the selection in a select field
onElementIDChange :: HasSelection option => String -> (option -> UI ()) -> UI ()
onElementIDChange elementId handler = do
  window   <- askWindow
  exported <- ffiExport $ \a -> void $ runUI window $ case fromSelectionLabel a of
                                                        Left  err    -> liftIO $ print err
                                                        Right option -> handler option
  runFunction $ ffi "$(%1).on('change', function(e) { var value = $(%1).prop('value').toString(); %2(value) })" ('#':elementId) exported
