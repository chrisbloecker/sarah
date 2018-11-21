{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material
  ( module Graphics.UI.Material
  )
  where
--------------------------------------------------------------------------------
import Control.Monad                   (forM_, when)
import Control.Monad.IO.Class          (MonadIO)
import Data.Monoid                     ((<>))
import Data.Maybe                      (isJust, fromJust)
import Data.Text                       (Text)
import Graphics.UI.Threepenny          (UI, runFunction, ffi)
import Sarah.GUI.Reactive
import Text.Blaze.Html.Renderer.String
--------------------------------------------------------------------------------
import Graphics.UI.Material.Class    as Graphics.UI.Material
import Graphics.UI.Material.Icon     as Graphics.UI.Material
import Graphics.UI.Material.Reactive as Graphics.UI.Material
import Graphics.UI.Material.Types    as Graphics.UI.Material
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Internal         as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

upgradeDom :: UI ()
upgradeDom = runFunction $ ffi "componentHandler.upgradeDom(); console.log('component upgrade ok.');"

toast :: Text -> UI ()
toast message = runFunction $ ffi script message
  where
    script = mconcat [ "console.log('Showing message in toast: %1');"
                     , "var toast = document.getElementById('toast');"
                     , "if (toast)"
                     , "  toast.MaterialSnackbar.showSnackbar({ message: %1 });"
                     ]

showDialogue :: String -> UI ()
showDialogue dialogueId =
  runFunction $ ffi "var dialogue = document.getElementById(%1); if (dialogue) dialogue.showModal();" dialogueId

hideDialogue :: String -> UI ()
hideDialogue dialogueId =
  runFunction $ ffi "var dialogue = document.getElementById(%1); if (dialogue) dialogue.close()" dialogueId

removeChildren :: String -> UI ()
removeChildren parentId = runFunction $ ffi script parentId
  where
    script = mconcat [ "var parent = document.getElementById(%1);"
                     , "while (parent.firstChild)"
                     , "{"
                     , "  parent.removeChild(parent.firstChild);"
                     , "}"
                     ]

--------------------------------------------------------------------------------

data Button = Button { item   :: H.Html
                     , itemId :: String
                     }

instance IsWidget Button where
  getItem   = item
  getItemId = itemId

button :: MonadIO m => Maybe Icon -> Maybe Text -> m Button
button micon mtext = do
  itemId <- newIdent

  let item = H.button H.! A.class_ (H.toValue ("mdl-button mdl-js-button" :: Text))
                      H.! A.id (H.toValue itemId) $ do
                 forM_ micon icon
                 forM_ mtext H.text

  return Button {..}

--------------------------------------------------------------------------------

data IconButton = IconButton { item   :: H.Html
                             , itemId :: String
                             }

instance IsWidget IconButton where
  getItem   = item
  getItemId = itemId

iconButton :: MonadIO m => Icon -> m IconButton
iconButton theIcon = do
  itemId <- newIdent

  let item = H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                      H.! A.id (H.toValue itemId) $
                 icon theIcon

  return IconButton{..}

--------------------------------------------------------------------------------

data NavigationLink = NavigationLink { item   :: H.Html
                                     , itemId :: String
                                     }

instance IsWidget NavigationLink where
  getItem   = item
  getItemId = itemId

navigationLink :: MonadIO m => Text -> m NavigationLink
navigationLink text = do
  itemId <- newIdent

  let item = H.a H.! A.class_ "mdl-navigation__link"
                 H.! A.id (H.toValue itemId)
                 H.! A.href "#" $
                 H.text text

  return NavigationLink{..}

--------------------------------------------------------------------------------

data Dialogue = Dialogue { item            :: H.Html
                         , itemId          :: String
                         , submitButtonId  :: String
                         , dismissButtonId :: String
                         }

instance IsWidget Dialogue where
  getItem   = item
  getItemId = itemId
instance HasSubmitButtonId  Dialogue where getSubmitButtonId  = submitButtonId
instance HasDismissButtonId Dialogue where getDismissButtonId = dismissButtonId

dialog = H.Parent "dialog" "<dialog" "</dialog>"
{-# INLINE dialog #-}

dialogue :: MonadIO m => Text -> H.Html -> m Dialogue
dialogue title content = do
  itemId <- newIdent

  submitButton  <- button Nothing (Just "Submit")
  dismissButton <- button Nothing (Just "Cancel")

  let submitButtonId  = getItemId submitButton
      dismissButtonId = getItemId dismissButton

      item = dialog H.! A.class_ "mdl-dialog"
                    H.! A.id (H.toValue itemId) $ do
               H.h4 H.! A.class_ "mdl-dialog__title" $
                   H.text title
               H.div H.! A.class_ "mdl-dialog__content" $
                   content
               H.div H.! A.class_ "mdl-dialog-actions" $ do
                   getItem submitButton
                   getItem dismissButton

  return Dialogue{..}

--------------------------------------------------------------------------------

data TileSize = TileSmall
              | TileMedium
              | TileLarge

mkTileSmall, mkTileMedium, mkTileLarge :: Text -> Maybe Text -> H.Html -> H.Html
mkTileSmall  = mkTile TileSmall
mkTileMedium = mkTile TileMedium
mkTileLarge  = mkTile TileLarge

mkTile :: TileSize -> Text -> Maybe Text -> H.Html -> H.Html
mkTile tileSize title mimg content =
    let (widthPhone, widthTablet, widthDesktop) = case tileSize of
                                                      TileSmall  -> ("3", "3",  "3")
                                                      TileMedium -> ("3", "5",  "5")
                                                      TileLarge  -> ("3", "7", "11")
        tileClass = unwords [ "mdl-cell"
                            , "mdl-cell--" ++ widthDesktop ++ "-col-desktop"
                            , "mdl-cell--" ++ widthTablet  ++ "-col-tablet"
                            , "mdl-cell--" ++ widthPhone   ++ "-col-phone"
                            ]
    in H.div H.! A.class_ (H.toValue tileClass) $
           H.div H.! A.class_ "mdl-card mdl-card-margin mdl-card--border mdl-shadow--4dp"
                 H.! A.style "width: 100%;" $ do
               H.div H.! A.class_ "mdl-card__title" $
                   H.h2 H.! A.class_ "mdl-card__title-text" $
                       H.text title
               when (isJust mimg) $
                   H.div H.! A.class_ "mdl-card__media" $
                       H.img H.! A.src (H.toValue . fromJust $ mimg)
                             H.! A.style "width: 100%;"
                             H.! A.alt ""
               H.div H.! A.class_ "mdl-card__actions mdl-card--border" $
                   content


list :: [H.Html] -> H.Html
list items = H.div $
                 H.ul H.! A.class_ "mdl-list" $
                     sequence_ items


listItem :: H.Html -> H.Html -> H.Html
listItem content action = H.li H.! A.class_ "mdl-list__item"
                               H.! A.style "height: 30px;" $ do
                              H.span H.! A.class_ "mdl-list__item-primary-content" $
                                  content
                              H.span H.! A.class_ "mdl-list__item-secondary-action" $
                                  action

table :: [Text] -> [H.Html] -> H.Html
table cols rows = H.table H.! A.class_ "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp" $ do
                      H.thead $
                          H.tr $ forM_ cols $ \col ->
                              H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $
                                  H.text col
                      H.tbody $ sequence_ rows

row :: [H.Html] -> H.Html
row cols = H.tr $ forM_ cols $ \col ->
               H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ col
