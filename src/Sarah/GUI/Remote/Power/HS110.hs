{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader    (lift, ask)
import Graphics.UI.Threepenny
import Sarah.GUI.Model
import Sarah.Middleware.Device (HS110)
--------------------------------------------------------------------------------

instance HasRemote HS110 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      string "renderRemote.HS110"
