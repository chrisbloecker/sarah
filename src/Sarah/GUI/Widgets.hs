{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Monad                 (when)
import Data.Maybe                    (isJust)
import Graphics.UI.Material
import Graphics.UI.Threepenny
import Prelude                hiding (div)
--------------------------------------------------------------------------------

data ReactiveButton = ReactiveButton { _elementRB :: Element }

instance Widget ReactiveButton where
  getElement = _elementRB

reactiveButton :: Behavior Class -> Behavior Style -> UI ReactiveButton
reactiveButton behaviourClass behaviourStyle = do
  display <- button

  element display # sink class_ (unClass <$> behaviourClass)
  element display # sink style  (unStyle <$> behaviourStyle)

  return ReactiveButton { _elementRB = display }


data ReactiveCheckbox = ReactiveCheckbox { _elementCB :: Element }

instance Widget ReactiveCheckbox where
  getElement = _elementCB

reactiveCheckbox :: Behavior Bool -> UI ReactiveCheckbox
reactiveCheckbox behaviour = do
  display <- input # set type_ "checkbox"

  element display # sink checked behaviour

  return ReactiveCheckbox { _elementCB = display }


data ReactiveLabel = ReactiveLabel { _elementRL :: Element }

instance Widget ReactiveLabel where
  getElement = _elementRL

reactiveLabel :: Behavior String -> UI ReactiveLabel
reactiveLabel behaviour = do
  display <- label
  element display # sink text behaviour
  return ReactiveLabel { _elementRL = display }


data ReactiveListItem = ReactiveListItem { _elementRLI :: Element }

instance Widget ReactiveListItem where
  getElement = _elementRLI

reactiveListItem :: Behavior Class -> UI ReactiveListItem
reactiveListItem behaviourClass = do
  display <- li

  element display # sink class_ (unClass <$> behaviourClass)

  return ReactiveListItem { _elementRLI = display }
