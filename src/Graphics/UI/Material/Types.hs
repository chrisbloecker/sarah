{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material.Types
  where
--------------------------------------------------------------------------------
import Graphics.UI.Threepenny (Behavior, Event, Handler, ToJS)
import Data.Text              (Text, pack)
import Text.Blaze.Html5       (Html)
--------------------------------------------------------------------------------

-- British English
type Behaviour = Behavior

class IsWidget a where
  getItem   :: a -> Html
  getItemId :: a -> String

class HasSubmitButtonId  a   where getSubmitButtonId  :: a -> String
class HasDismissButtonId a   where getDismissButtonId :: a -> String

class IsReactive a t where
  getEvent     :: a -> Event     t
  getHandler   :: a -> Handler   t
  getBehaviour :: a -> Behaviour t
