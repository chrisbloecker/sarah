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

newtype Style = Style { unStyle :: [(String, String)] }

upgradeDom :: UI ()
upgradeDom = runFunction $ ffi "componentHandler.upgradeDom();console.log('component upgrade ok.')"


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


data List = List { _elementList :: Element }

instance Widget List where
  getElement = _elementList

list :: [UI ListItem] -> UI List
list items = do
  elem <- div #+ [ ul # set class_ (unClass mdl_list)
                     #+ map (fmap getElement) items
                 ]

  return List { _elementList = elem }


data ListItem = ListItem { _elementListItem :: Element }

instance Widget ListItem where
  getElement = _elementListItem

listItem :: (Widget widget0, Widget widget1) => UI widget0 -> UI widget1 -> UI ListItem
listItem content action = do
  elem <- li # set class_ (unClass mdl_list_item)
             #+ [ span # set class_ (unClass mdl_list_item_primary_content)  #+ [ getElement <$> content ]
                , span # set class_ (unClass mdl_list_item_secondary_action) #+ [ getElement <$> action  ]
                ]

  return ListItem { _elementListItem = elem }


-- A toggle is a decorator for a checkbox
data Toggle = Toggle { _elementToggle :: Element }

instance Widget Toggle where
  getElement = _elementToggle

toggle :: UI Element -> UI Toggle
toggle checkbox = do
  newId <- toString <$> liftIO nextRandom
  elem <- label # set class_ (unClass $ buildClass [mdl_switch, mdl_js_switch, mdl_js_ripple_effect])
                # set for newId
                #+ [ checkbox # set class_ (unClass mdl_switch_input)
                              # set id_ newId
                   , span # set class_ (unClass mdl_switch_label)
                   ]

  return Toggle { _elementToggle = elem }
