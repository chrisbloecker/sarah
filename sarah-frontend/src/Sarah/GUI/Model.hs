{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Model
  where
--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader        (ReaderT)
import           Control.Monad.State         (StateT, MonadState)
import           Graphics.UI.Threepenny.Core
import           Network.HTTP.Client         (Manager)
import           Servant.Common.BaseUrl      (BaseUrl)
--------------------------------------------------------------------------------

data AppEnv = AppEnv { _manager    :: Manager
                     , _middleware :: BaseUrl
                     }

makeLenses ''AppEnv

type AppT m = ReaderT AppEnv m
type AppIO  = AppT IO

{-
data Page = Page { _tiles   :: ![UI Element]
                 , _actions :: ![UI ()]
                 }

makeLenses ''Page

type PageBuilder = StateT Page AppIO

addPageTile :: MonadState Page m => UI Element -> m ()
addPageTile tile = tiles %= (tile :)

addPageUIAction :: MonadState Page m => UI () -> m ()
addPageUIAction action = actions %= (action :)
-}
