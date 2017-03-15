{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Monad                 (when)
import Data.Maybe                    (isJust)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny
import Prelude                hiding (div)
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


data ReactiveProgressBar = ReactiveProgressBar { _elementRPB :: Element }

instance Widget ReactiveProgressBar where
  getElement = _elementRPB

reactiveProgressBar :: Behavior (Integer, String) -> UI ReactiveProgressBar
reactiveProgressBar behaviour = do
  display <- string ""
  inner   <- div # set class_ "progress-bar"
                 # set (attr "role") "progressbar"
                 # set (attr "aria-valuemin") "0"
                 # set (attr "aria-valuemax") "100"
                 #+ [ div # set class_ "text-center"
                          #+ [ element display ]
                    ]

  progressBar <- div # set class_ "progress"
                     # set style [("width", "200px")]
                     #+ [ element inner ]

  element inner   # sink (attr "aria-valuenow") (show . fst <$> behaviour)
  element display # sink text                   (       snd <$> behaviour)

  return ReactiveProgressBar { _elementRPB = progressBar }
