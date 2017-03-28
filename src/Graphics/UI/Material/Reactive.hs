module Graphics.UI.Material.Reactive
  where
--------------------------------------------------------------------------------
import Control.Monad                      (forM)
import Data.UUID                          (toString)
import Data.UUID.V4                       (nextRandom)
import Graphics.UI.Material.Class
import Graphics.UI.Material.Icon
import Graphics.UI.Threepenny      hiding (map, empty)
import Graphics.UI.Threepenny.Core        (runFunction, ffi)
import Prelude                     hiding (span, div)
--------------------------------------------------------------------------------

newIdent :: MonadIO m => m String
newIdent = toString <$> liftIO nextRandom


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
