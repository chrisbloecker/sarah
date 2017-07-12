{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material
  ( module Graphics.UI.Material
  )
  where
--------------------------------------------------------------------------------
import Control.Monad          (forM_)
import Data.Text              (Text)
import Graphics.UI.Threepenny
import Sarah.GUI.Reactive
--------------------------------------------------------------------------------
import Graphics.UI.Material.Class    as Graphics.UI.Material
import Graphics.UI.Material.Icon     as Graphics.UI.Material
import Graphics.UI.Material.Reactive as Graphics.UI.Material
import Graphics.UI.Material.Types    as Graphics.UI.Material
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

upgradeDom :: UI ()
upgradeDom = runFunction $ ffi "componentHandler.upgradeDom();console.log('component upgrade ok.')"


data Button = Button { item   :: H.Html
                     , itemId :: String
                     }

instance HasItem   Button where getItem   = item
instance HasItemId Button where getItemId = itemId

button :: MonadIO m => Maybe Icon -> Maybe Text -> m Button
button micon mtext = do
  itemId <- newIdent

  let item = H.button H.! A.class_ (H.toValue ("mdl-button mdl-js-button" :: Text))
                      H.! A.id (H.toValue itemId) $ do
                 forM_ micon icon
                 forM_ mtext H.text

  return Button {..}


mkTile :: Text -> H.Html -> H.Html
mkTile title content = H.div H.! A.class_ "mdl-card mdl-card-margin mdl-card--border mdl-shadow--2dp" $ do
                           H.div H.! A.class_ "mdl-card__title" $
                               H.h2 H.! A.class_ "mdl-card__title-text" $
                               H.text title
                           H.div H.! A.class_ "mdl-card__actions mdl-card--border" $
                               content


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
