{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material.Types
  where
--------------------------------------------------------------------------------
import Graphics.UI.Threepenny (UI, Behavior, Event, Handler, ToJS)
import Data.Text              (Text, pack)
import Text.Blaze.Html5       (Html)
--------------------------------------------------------------------------------

-- British English
type Behaviour = Behavior

class IsWidget a where
  getItem   :: a -> Html
  getItemId :: a -> String

class IsWidget a => HasInput a t where
  getInput :: a -> UI (Maybe t)

class IsWidget a => IsReactive a t where
  getEvent     :: a -> Event     t
  getHandler   :: a -> Handler   t
  getBehaviour :: a -> Behaviour t

class IsWidget a => HasPageActions a where
  getPageActions :: a -> [UI ()]

class HasSubmitButtonId  a where
  getSubmitButtonId :: a -> String

class HasDismissButtonId a where
  getDismissButtonId :: a -> String
