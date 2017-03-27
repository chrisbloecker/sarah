module Graphics.UI.Material.Reactive
  where
--------------------------------------------------------------------------------
import Data.UUID                          (toString)
import Data.UUID.V4                       (nextRandom)
import Graphics.UI.Material.Class
import Graphics.UI.Threepenny      hiding (map)
import Graphics.UI.Threepenny.Core        (runFunction, ffi)
import Prelude                     hiding (span)
--------------------------------------------------------------------------------

newIdent :: MonadIO m => m String
newIdent = toString <$> liftIO nextRandom

-- A toggle is a decorator for a checkbox
data ReactiveToggle = ReactiveToggle { _elementReactiveToggle :: Element
                                     , getCheckbox            :: Element
                                     }

instance Widget ReactiveToggle where
  getElement = _elementReactiveToggle

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

  return ReactiveToggle { _elementReactiveToggle = toggle
                        , getCheckbox            = checkbox
                        }
