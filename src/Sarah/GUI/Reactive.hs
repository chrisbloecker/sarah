module Sarah.GUI.Reactive
  where

import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.UUID                   (toString)
import Data.UUID.V4                (nextRandom)
import Graphics.UI.Threepenny      (UI, runUI, askWindow)
import Graphics.UI.Threepenny.Core (runFunction, ffi, ffiExport)

newIdent :: MonadIO m => m String
newIdent = toString <$> liftIO nextRandom

-- taken from hue-dashboard by blitzcode
onElementIDClick :: String -> UI void -> UI ()
onElementIDClick elementID handler = do
    window   <- askWindow
    exported <- ffiExport $ runUI window handler >> return ()
    runFunction $ ffi "$(%1).on('click', %2)" ("#" ++ elementID) exported
