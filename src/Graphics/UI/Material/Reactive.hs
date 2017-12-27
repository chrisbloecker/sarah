{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Graphics.UI.Material.Reactive
  where
--------------------------------------------------------------------------------
import Control.Monad                      (forM_, when, void)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Data.Text                          (Text, pack, unpack)
import Graphics.UI.Material.Types
import Graphics.UI.Material.Icon
import Graphics.UI.Threepenny             (Event, Handler, UI, newEvent, stepper, currentValue, onChanges)
import Graphics.UI.Threepenny.Core        (runFunction, ffi)
import Prelude                     hiding (span, div)
import Sarah.GUI.Reactive
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
--------------------------------------------------------------------------------

data Dropdown = Dropdown { item   :: H.Html
                         , itemId :: String
                         }

instance IsWidget Dropdown where
  getItem   = item
  getItemId = itemId

dropdown :: MonadIO m => H.Html -> [H.Html] -> m Dropdown
dropdown label items = do
  itemId <- newIdent

  let item = H.div $ do
                 label
                 H.button H.! A.class_ "mdl-button mdl-js-button mdl-button--icon"
                          H.! A.id (H.toValue itemId) $
                              icon arrow_drop_up
                 H.ul H.! A.class_ "mdl-menu mdl-menu--top-right mdl-js-menu mdl-js-ripple-effect"
                      H.! A.for (H.toValue itemId) $
                          sequence_ items

  return Dropdown{..}

--------------------------------------------------------------------------------

data ReactiveLabel = ReactiveLabel { item      :: H.Html
                                   , itemId    :: String
                                   , event     :: Event     Text
                                   , handler   :: Handler   Text
                                   , behaviour :: Behaviour Text
                                   }

instance IsWidget ReactiveLabel where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveLabel Text where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveLabel :: (MonadIO m, m ~ UI) => Text -> UI ReactiveLabel
reactiveLabel initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialText      <- currentValue behaviour

  let item = H.label H.! A.id (H.toValue itemId) $
                 H.text initialText

  onChanges behaviour $ \newText ->
    runFunction $ ffi "$(%1).text(%2);" ('#':itemId) newText

  return ReactiveLabel{..}

--------------------------------------------------------------------------------

data ReactiveToggle = ReactiveToggle { item      :: H.Html
                                     , itemId    :: String
                                     , event     :: Event Bool
                                     , handler   :: Handler Bool
                                     , behaviour :: Behaviour Bool
                                     }

instance IsWidget ReactiveToggle where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveToggle Bool where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveToggle :: Bool -> UI ReactiveToggle
reactiveToggle initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  labelId          <- newIdent
  itemId           <- newIdent
  initialChecked   <- currentValue behaviour

  let checkbox = H.input H.! A.class_ "mdl-switch__input"
                     H.! A.type_ "checkbox"
                     H.! A.id (H.toValue itemId)
                     H.! A.checked (H.toValue initialChecked)

      item = H.label H.! A.class_ "mdl-switch mdl-js-switch mdl-js-ripple-effect"
                     H.! A.id (H.toValue labelId)
                     H.! A.for (H.toValue itemId) $ do
                         checkbox
                         H.span H.! A.class_ "mdl-switch__label" $ ""

  onChanges behaviour $ \newChecked -> do
    runFunction $ ffi "$(%1).prop('checked', %2);" ('#':itemId) newChecked
    if newChecked
      then runFunction $ ffi "var elem = $(%1); if (elem && elem.hasClass('is-upgraded')) elem[0].MaterialSwitch.on();"  ('#':labelId)
      else runFunction $ ffi "var elem = $(%1); if (elem && elem.hasClass('is-upgraded')) elem[0].MaterialSwitch.off();" ('#':labelId)

  return ReactiveToggle {..}

--------------------------------------------------------------------------------

data ReactiveButton = ReactiveButton { item   :: H.Html
                                     , itemId :: String
                                     }

instance IsWidget ReactiveButton where
  getItem   = item
  getItemId = itemId

reactiveButton :: Text -> Behaviour Text -> UI ReactiveButton
reactiveButton label behaviour = do
  itemId       <- newIdent
  initialClass <- currentValue behaviour

  let item = H.button H.! A.class_ (H.toValue initialClass)
                      H.! A.id (H.toValue itemId) $
                 H.text label

  onChanges behaviour $ \newClass ->
    runFunction $ ffi "$(%1).prop('class',%2);" ('#':itemId) newClass

  return ReactiveButton{..}

--------------------------------------------------------------------------------

data ReactiveCheckbox = ReactiveCheckbox { item   :: H.Html
                                         , itemId :: String
                                         }

instance IsWidget ReactiveCheckbox where
  getItem   = item
  getItemId = itemId

reactiveCheckbox :: Behaviour Bool -> UI ReactiveCheckbox
reactiveCheckbox behaviour = do
  itemId         <- newIdent
  initialChecked <- currentValue behaviour

  let item = H.input H.! A.type_ "checkbox"
                     H.! A.id (H.toValue itemId)
                     H.! A.checked (H.toValue initialChecked)

  onChanges behaviour $ \newChecked ->
    runFunction $ ffi "$(%1).prop('checked',%2);" ('#':itemId) newChecked

  return ReactiveCheckbox{..}

--------------------------------------------------------------------------------

data ReactiveOption = ReactiveOption { item   :: H.Html
                                     , itemId :: String
                                     }

instance IsWidget ReactiveOption where
  getItem   = item
  getItemId = itemId

reactiveOption :: HasSelection option => option -> UI ReactiveOption
reactiveOption option = do
  itemId <- newIdent

  let item = H.option H.! A.value (H.toValue . toSelectionLabel $ option) $
                 H.text (toSelectionLabel option)

  return ReactiveOption{..}

--------------------------------------------------------------------------------

data ReactiveSelectField option = ReactiveSelectField { item      :: H.Html
                                                      , itemId    :: String
                                                      , event     :: Event     option
                                                      , handler   :: Handler   option
                                                      , behaviour :: Behaviour option
                                                      }

instance HasSelection option => IsWidget (ReactiveSelectField option) where
  getItem   = item
  getItemId = itemId

instance HasSelection option => IsReactive (ReactiveSelectField option) option where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveSelectField :: HasSelection option
                    => [ReactiveOption] -> option -> UI (ReactiveSelectField option)
reactiveSelectField options initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialSelected  <- currentValue behaviour

  let item = H.div H.! A.class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
                 H.select H.! A.class_ "mdl-textfield__input"
                          H.! A.id (H.toValue itemId) $
                     forM_ options getItem
                 H.label H.! A.class_ "mdl-textfield__label"
                         H.! A.for (H.toValue itemId) $
                     H.text (toSelectionLabel initial)

  onChanges behaviour $ \newSelection ->
    runFunction $ ffi "document.getElementById(%1).value = %2; console.log(%2);" itemId (toSelectionLabel newSelection)

  return ReactiveSelectField{..}

--------------------------------------------------------------------------------

data ReactiveListItem = ReactiveListItem { item      :: H.Html
                                         , itemId    :: String
                                         , event     :: Event     Text
                                         , handler   :: Handler   Text
                                         , behaviour :: Behaviour Text
                                         }

instance IsWidget ReactiveListItem where
  getItem   = item
  getItemId = itemId

instance IsReactive ReactiveListItem Text where
  getEvent     = event
  getHandler   = handler
  getBehaviour = behaviour

reactiveListItem :: Text -> Text -> UI ReactiveListItem
reactiveListItem label initial = do
  (event, handler) <- liftIO newEvent
  behaviour        <- stepper initial event
  itemId           <- newIdent
  initialClass     <- currentValue behaviour

  let item = H.li H.! A.class_ "mdl-list__item" $
                 H.div H.! A.id (H.toValue itemId)
                       H.! A.class_ (H.toValue initialClass) $
                     H.text label

  onChanges behaviour $ \newClass ->
    runFunction $ ffi "$(%1).prop('class',%2);" ('#':itemId) newClass

  return ReactiveListItem{..}
