{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Material
  ( module Graphics.UI.Material
  )
  where
--------------------------------------------------------------------------------
import Control.Monad                     (forM)
import Data.UUID                         (toString)
import Data.UUID.V4                      (nextRandom)
import Graphics.UI.Threepenny     hiding (map)
import Prelude                    hiding (div, span)
--------------------------------------------------------------------------------
import Graphics.UI.Material.Class    as Graphics.UI.Material
import Graphics.UI.Material.Icon     as Graphics.UI.Material
import Graphics.UI.Material.Reactive as Graphics.UI.Material
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

upgradeDom :: UI ()
upgradeDom = runFunction $ ffi "componentHandler.upgradeDom();console.log('component upgrade ok.')"


list :: [H.Html] -> H.Html
list items = H.div $
                 H.ul H.! A.class_ "mdl-list" $
                     sequence_ items


listItem :: H.Html -> H.Html -> H.Html
listItem content action = H.li H.! A.class_ "mdl-list__item" $ do
                              H.span H.! A.class_ "mdl-list__item-primary-content" $
                                  content
                              H.span H.! A.class_ "mdl-list__item-secondary-action" $
                                  action


slider :: Int -> Int -> Int -> Int -> H.Html
slider width min max value = H.p H.! A.style (H.toValue . unwords $ ["width:", show width ++ "px"]) $
                                 H.input H.! A.class_ "mdl-slider mdl-js-slider"
                                         H.! A.type_ "range"
                                         H.! A.min (H.toValue min)
                                         H.! A.max (H.toValue max)
                                         H.! A.value (H.toValue value)
                                         H.! A.step (H.toValue (1 :: Int))
