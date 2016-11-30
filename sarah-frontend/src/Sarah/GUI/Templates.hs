{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Templates
  where
--------------------------------------------------------------------------------
import           Prelude                     hiding (div, span, id)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (span)
--------------------------------------------------------------------------------
--import qualified Text.Blaze.Html5 as H
--------------------------------------------------------------------------------

mkNavbar :: Html
mkNavbar = nav ! class_ "navbar navbar-default" $
             div ! class_ "container-fluid" $ do
               div ! class_ "navbar-header" $
                 button ! type_ "button"
                        ! class_ "navbar-toggle collapsed"
                        ! dataAttribute "target" "the-navbar"
                        ! customAttribute "aria-expanded" "false" $ do
                   span ! class_ "icon-bar" $ ""
                   span ! class_ "icon-bar" $ ""
                   span ! class_ "icon-bar" $ ""
               div ! class_ "collapse navbar-collapse"
                   ! id "the-navbar"
