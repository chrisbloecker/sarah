--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Prelude                      hiding (div, span, id)
import Graphics.UI.Threepenny       hiding (map)
--------------------------------------------------------------------------------

data ReactiveLabel = ReactiveLabel { _elementRL :: Element }

reactiveLabel :: Behavior String -> UI ReactiveLabel
reactiveLabel behaviour = do
  display <- label
  element display # sink text behaviour
  return ReactiveLabel { _elementRL = display }

instance Widget ReactiveLabel where
  getElement = _elementRL
