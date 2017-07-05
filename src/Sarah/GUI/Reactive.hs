module Sarah.GUI.Reactive
  where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.UUID                   (toString)
import Data.UUID.V4                (nextRandom)
import Graphics.UI.Threepenny      (UI, runUI, askWindow)
import Graphics.UI.Threepenny.Core (runFunction, ffi, ffiExport)
--------------------------------------------------------------------------------

newIdent :: MonadIO m => m String
newIdent = toString <$> liftIO nextRandom

toBool :: String -> Bool
toBool "true" = True
toBool _      = False

-- taken from hue-dashboard by blitzcode
onElementIDClick :: String -> UI () -> UI ()
onElementIDClick elementID handler = do
    window   <- askWindow
    exported <- ffiExport $ runUI window handler >> return ()
    runFunction $ ffi "$(%1).on('click', %2);" ('#':elementID) exported

-- event that occurs when the user changed the checked state of a checkbox
onElementIDCheckedChange :: String -> (Bool -> UI ()) -> UI ()
onElementIDCheckedChange elementId handler = do
  window   <- askWindow
  exported <- ffiExport (\s -> runUI window (handler . toBool $ s) >> return ())
  runFunction $ ffi "$(%1).on('change', function(e) { var checked = $(%1).prop('checked').toString(); %2(checked) } )" ('#':elementId) exported
