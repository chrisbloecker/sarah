{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
module Graphics.UI.Material.Types
  where
--------------------------------------------------------------------------------
import Graphics.UI.Threepenny
import Text.Blaze.Html5
--------------------------------------------------------------------------------

type Behaviour = Behavior

class HasItem            a   where getItem            :: a -> Html
class HasItemId          a   where getItemId          :: a -> String
class HasSubmitButtonId  a   where getSubmitButtonId  :: a -> String
class HasDismissButtonId a   where getDismissButtonId :: a -> String
class HasEvent           a t where getEvent           :: a -> Event     t
class HasHandler         a t where getHandler         :: a -> Handler   t
class HasBehaviour       a t where getBehaviour       :: a -> Behaviour t
