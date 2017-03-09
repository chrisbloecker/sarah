{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Monad          (when)
import Data.Maybe             (isJust)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny
--------------------------------------------------------------------------------

data ReactiveLabel = ReactiveLabel { _elementRL :: Element }

instance Widget ReactiveLabel where
  getElement = _elementRL

reactiveLabel :: Behavior String -> UI ReactiveLabel
reactiveLabel behaviour = do
  display <- label
  element display # sink text behaviour
  return ReactiveLabel { _elementRL = display }


data ReactiveButton = ReactiveButton { _elementRB :: Element }

instance Widget ReactiveButton where
  getElement = _elementRB

reactiveButton :: Behavior Class -> Behavior Style -> UI ReactiveButton
reactiveButton behaviourClass behaviourStyle = do
  display <- button

  element display # sink class_ (unClass <$> behaviourClass)
  element display # sink style  (unStyle <$> behaviourStyle)

  return ReactiveButton { _elementRB = display }
