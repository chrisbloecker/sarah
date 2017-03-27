module Graphics.UI.Bootstrap.Builder
  where

import Graphics.UI.Material.Class
import Graphics.UI.Bootstrap.Glyphicon (Glyphicon (..))
import Graphics.UI.Threepenny
import Prelude hiding (span)

bootstrapButton (Class cls) (Glyphicon glyph) = button # set class_ cls #+ [ span # set class_ glyph ]
