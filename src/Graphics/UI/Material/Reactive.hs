{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Graphics.UI.Material.Reactive
  where
--------------------------------------------------------------------------------
import Control.Monad                      (forM, void)
import Data.Text                          (Text, pack, unpack)
import Graphics.UI.Material.Class
import Graphics.UI.Material.Icon
import Graphics.UI.Threepenny      hiding (map, empty)
import Graphics.UI.Threepenny.Core        (runFunction, ffi)
import Prelude                     hiding (span, div)
import Sarah.GUI.Reactive
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

f <$$> e = fmap (f . element) <$> e

dropdown :: H.Html -> [H.Html] -> UI H.Html
dropdown label items = do
  buttonId <- H.toValue <$> newIdent
  let item = H.div $ do
                 label
                 H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                          H.! A.id buttonId $
                              icon arrow_drop_up
                 H.ul H.! A.class_ "mdl-menu mdl-menu--top-right mdl-js-menu mdl-js-ripple-effect"
                      H.! A.for buttonId $
                          sequence_ items

  return item


reactiveLabel :: Behavior Text -> UI H.Html
reactiveLabel behaviour = do
  window      <- askWindow
  labelId     <- newIdent
  initialText <- currentValue behaviour

  let item = H.label H.! A.id (H.toValue labelId) $
                 H.text initialText

  onChanges behaviour $ \newText ->
    runFunction $ ffi "$(%1).text(%2);" ('#':labelId) newText

  return item


reactiveToggle :: Behavior Bool -> UI (H.Html, String)
reactiveToggle behaviour = do
  window         <- askWindow
  labelId        <- newIdent
  checkboxId     <- newIdent
  initialChecked <- currentValue behaviour

  let checkbox = H.input H.! A.class_ "mdl-switch__input"
                     H.! A.type_ "checkbox"
                     H.! A.id (H.toValue checkboxId)
                     H.! A.checked (H.toValue initialChecked)

      toggle = H.label H.! A.class_ "mdl-switch mdl-js-switch mdl-js-ripple-effect"
                       H.! A.id (H.toValue labelId)
                       H.! A.for (H.toValue checkboxId) $ do
                           checkbox
                           H.span H.! A.class_ "mdl-switch__label" $ ""

  onChanges behaviour $ \newChecked ->
    runFunction $ ffi "$(%1).prop('checked', %2);" ('#':checkboxId) newChecked
    {- For some reason, this kills the browser
    if newChecked
      then runFunction $ ffi "$(%1)[0].MaterialSwitch.on();"  ('#':labelId)
      else runFunction $ ffi "$(%1)[0].MaterialSwitch.off();" ('#':labelId)
    -}

  return (toggle, checkboxId)


newtype ReactiveButton = ReactiveButton { _elementRB :: H.Html }

reactiveButton :: Behavior Text -> UI ReactiveButton
reactiveButton behaviour = do
  window       <- askWindow
  buttonId     <- newIdent
  initialClass <- currentValue behaviour

  let display = H.button H.! A.class_ (H.toValue initialClass)
                         H.! A.id (H.toValue buttonId) $
                             ""

  onChanges behaviour $ \newClass ->
    runFunction $ ffi "$(%1).prop('class',%2);" ('#':buttonId) newClass

  return ReactiveButton { _elementRB = display }


newtype ReactiveCheckbox = ReactiveCheckbox { _elementCB :: H.Html }

reactiveCheckbox :: Behavior Bool -> UI ReactiveCheckbox
reactiveCheckbox behaviour = do
  window         <- askWindow
  checkboxId     <- newIdent
  initialChecked <- currentValue behaviour

  let display = H.input H.! A.type_ "checkbox"
                        H.! A.id (H.toValue checkboxId)
                        H.! A.checked (H.toValue initialChecked)

  onChanges behaviour $ \newChecked ->
    runFunction $ ffi "$(%1).prop('checked',%2);" ('#':checkboxId) newChecked

  return ReactiveCheckbox { _elementCB = display }


reactiveListItem :: Text -> Behavior Text -> UI (H.Html, String)
reactiveListItem label behaviour = do
  window       <- askWindow
  itemId       <- newIdent
  initialClass <- currentValue behaviour

  let item = H.li H.! A.class_ "mdl-list__item" $
                 H.div H.! A.id (H.toValue itemId)
                       H.! A.class_ (H.toValue initialClass) $
                     H.text label

  onChanges behaviour $ \newClass ->
    runFunction $ ffi "$(%1).prop('class',%2);" ('#':itemId) newClass

  return (item, itemId)
