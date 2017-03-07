module Sarah.GUI.Remote.Power.HS110
  where

import Graphics.UI.Threepenny
import Sarah.GUI.Model         (HasRemote (..))
import Sarah.Middleware.Device (HS110)

instance HasRemote HS110 where
  renderRemote appEnv deviceAddress _ = string "renderRemote.HS110"
