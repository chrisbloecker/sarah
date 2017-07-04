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

newtype Dropdown = Dropdown { _elementDropdown :: H.Html }

dropdown :: H.Html -> [H.Html] -> UI Dropdown
dropdown label items = do
  buttonId <- H.toValue <$> newIdent
  let elem = H.div $ do
                 label
                 H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                          H.! A.id buttonId $
                              icon arrow_drop_up
                 H.ul H.! A.class_ "mdl-menu mdl-menu--top-right mdl-js-menu mdl-js-ripple-effect"
                      H.! A.for buttonId $
                          sequence_ items

  return Dropdown { _elementDropdown = elem }


newtype ReactiveLabel = ReactiveLabel { _elementRL :: H.Html }

reactiveLabel :: Behavior Text -> UI ReactiveLabel
reactiveLabel behaviour = do
  window      <- askWindow
  labelId     <- newIdent
  initialText <- currentValue behaviour

  let display = H.label H.! A.id (H.toValue labelId) $
                    H.text initialText

  -- ToDo: add sink
  --sink text (fmap unpack behaviour) <$$> getElementById window labelId
  onChanges behaviour $ \newText -> do
    liftIO $ putStrLn "Label changed"
    runFunction $ ffi "$(%1).innerHtml = %2" labelId newText

  return ReactiveLabel { _elementRL = display }


-- A toggle is a decorator for a checkbox
data ReactiveToggle = ReactiveToggle { getToggle   :: H.Html
                                     , getCheckbox :: H.Html
                                     }

reactiveToggle :: Behavior Bool -> UI ReactiveToggle
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

  -- ToDo: add sink
  --sink checked behaviour <$$> getElementById window checkboxId
  onChanges behaviour $ \newChecked -> do
    liftIO $ putStrLn "Toggle changed"
    runFunction $ ffi "$(%1).checked = %2" checkboxId newChecked

  onChanges behaviour $ \checked -> runFunction . ffi $ if checked
                                                          then "$('#" ++ labelId ++ "')[0].MaterialSwitch.on()"
                                                          else "$('#" ++ labelId ++ "')[0].MaterialSwitch.off()"

  return ReactiveToggle { getToggle   = toggle
                        , getCheckbox = checkbox
                        }


newtype ReactiveButton = ReactiveButton { _elementRB :: H.Html }

reactiveButton :: Behavior Text -> UI ReactiveButton
reactiveButton behaviour = do
  window       <- askWindow
  buttonId     <- newIdent
  initialClass <- currentValue behaviour

  let display = H.button H.! A.class_ (H.toValue initialClass)
                         H.! A.id (H.toValue buttonId) $
                             ""

  -- ToDo: add sink
  --sink class_ (fmap unpack behaviour) <$$> getElementById window buttonId
  onChanges behaviour $ \newClass -> do
    liftIO $ putStrLn "Button changed"
    runFunction $ ffi "$(%1).class = %2" buttonId newClass

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

  -- ToDo: add sink
  --sink checked behaviour <$$> getElementById window checkboxId
  onChanges behaviour $ \newChecked -> do
    liftIO $ putStrLn "Checkbox changed"
    runFunction $ ffi "$(%1).checked = %2" checkboxId newChecked

  return ReactiveCheckbox { _elementCB = display }


data ReactiveListItem = ReactiveListItem { item   :: H.Html
                                         , itemId :: String
                                         }

reactiveListItem :: Text -> Behavior Text -> UI ReactiveListItem
reactiveListItem label behaviour = do
  window       <- askWindow
  listItemId   <- newIdent
  initialClass <- currentValue behaviour

  let display = H.li H.! A.id (H.toValue listItemId)
                     H.! A.class_ (H.toValue initialClass) $
                         H.text label

  -- ToDo: add sink
  --sink class_ (fmap unpack behaviour) <$$> getElementById window listItemId
  onChanges behaviour $ \newClass -> do
    liftIO $ putStrLn "ListItem changed"
    runFunction $ ffi "$(%1).class = %2" listItemId newClass

  return ReactiveListItem { item   = display
                          , itemId = listItemId
                          }

{-
data Dropdown = Dropdown { _elementDropdown :: Element }

instance Widget Dropdown where
  getElement = _elementDropdown

dropdown :: (Widget widget0, Widget widget1) => UI widget0 -> [UI widget1] -> UI Dropdown
dropdown label items = do
  newId <- toString <$> liftIO nextRandom
  elem <- div #+ [ fmap getElement label
                 , button # set class_ (unClass $ buildClass [mdl_button, mdl_js_button, mdl_button_icon])
                          # set id_ newId
                          #+ [ icon arrow_drop_up ]
                 , ul # set class_ (unClass $ buildClass [mdl_menu, mdl_menu_top_right, mdl_js_menu, mdl_js_ripple_effect])
                      # set for newId
                      #+ fmap (\item -> li # set class_ (unClass mdl_menu_item) #+ [ getElement <$> item ]) items
                 ]

  return Dropdown { _elementDropdown = elem }


data ReactiveLabel = ReactiveLabel { _elementRL :: Element }

instance Widget ReactiveLabel where
  getElement = _elementRL

reactiveLabel :: Behavior String -> UI ReactiveLabel
reactiveLabel behaviour = do
  display <- label
  element display # sink text behaviour
  return ReactiveLabel { _elementRL = display }



-- A toggle is a decorator for a checkbox
data ReactiveToggle = ReactiveToggle { getToggle   :: Element
                                     , getCheckbox :: Element
                                     }

instance Widget ReactiveToggle where
  getElement = getToggle

reactiveToggle :: Behavior Bool -> UI ReactiveToggle
reactiveToggle behaviour = do
  labelId    <- newIdent
  checkboxId <- newIdent

  checkbox <- input # set class_ (unClass mdl_switch_input)
                    # set type_ "checkbox"
                    # set id_ checkboxId
  toggle <- label # set class_ (unClass $ buildClass [mdl_switch, mdl_js_switch, mdl_js_ripple_effect])
                  # set id_ labelId
                  # set for checkboxId
                  #+ [ element checkbox
                     , span # set class_ (unClass mdl_switch_label)
                     ]

  element checkbox # sink checked behaviour

  onChanges behaviour $ \checked -> runFunction . ffi $ if checked
                                                          then "$('#" ++ labelId ++ "')[0].MaterialSwitch.on()"
                                                          else "$('#" ++ labelId ++ "')[0].MaterialSwitch.off()"

  return ReactiveToggle { getToggle   = toggle
                        , getCheckbox = checkbox
                        }

data ReactiveButton = ReactiveButton { _elementRB :: Element }

instance Widget ReactiveButton where
  getElement = _elementRB

reactiveButton :: Behavior Class -> UI ReactiveButton
reactiveButton behaviourClass = do
  display <- button

  element display # sink class_ (unClass <$> behaviourClass)

  return ReactiveButton { _elementRB = display }


data ReactiveCheckbox = ReactiveCheckbox { _elementCB :: Element }

instance Widget ReactiveCheckbox where
  getElement = _elementCB

reactiveCheckbox :: Behavior Bool -> UI ReactiveCheckbox
reactiveCheckbox behaviour = do
  display <- input # set type_ "checkbox"

  element display # sink checked behaviour

  return ReactiveCheckbox { _elementCB = display }


data ReactiveListItem = ReactiveListItem { _elementRLI :: Element }

instance Widget ReactiveListItem where
  getElement = _elementRLI

reactiveListItem :: Behavior Class -> UI ReactiveListItem
reactiveListItem behaviourClass = do
  display <- li

  element display # sink class_ (unClass <$> behaviourClass)

  return ReactiveListItem { _elementRLI = display }
-}
