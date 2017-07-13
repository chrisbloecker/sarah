{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Schedule
  ( buildSchedule
  ) where
--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Threepenny (UI)
import Sarah.GUI.Model        (AppEnv (..))
import Sarah.GUI.Websocket    (toMaster)
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

buildSchedule :: AppEnv -> UI H.Html
buildSchedule AppEnv{..} = do
  schedule <- liftIO $ toMaster middleware GetStatusRequest

  return undefined
