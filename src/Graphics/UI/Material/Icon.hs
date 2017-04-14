module Graphics.UI.Material.Icon
  where

import Graphics.UI.Threepenny

newtype Icon = Icon { unIcon :: String }

icon :: Icon -> UI Element
icon i = mkElement "i" # set class_ "material-icons" # set text (unIcon i)

arrow_drop_down = Icon "arrow_drop_down"
arrow_drop_up = Icon "arrow_drop_up"
bug_report = Icon "bug_report"
chevron_left = Icon "chevron_left"
chevron_right = Icon "chevron_right"
trending_up = Icon "trending_up"
refresh = Icon "refresh"
