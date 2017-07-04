{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Material.Icon
  where

--------------------------------------------------------------------------------
import Data.Text (Text)
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

newtype Icon = Icon { unIcon :: Text }

icon :: Icon -> H.Html
icon i = H.i H.! A.class_ "material-icons" $ H.text (unIcon i)

arrow_drop_down = Icon "arrow_drop_down"
arrow_drop_up = Icon "arrow_drop_up"
bug_report = Icon "bug_report"
chevron_left = Icon "chevron_left"
chevron_right = Icon "chevron_right"
trending_up = Icon "trending_up"
refresh = Icon "refresh"
