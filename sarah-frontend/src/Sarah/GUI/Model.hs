module Sarah.GUI.Model
  where
--------------------------------------------------------------------------------
import           Network.HTTP.Client    (Manager)
import           Servant.Common.BaseUrl (BaseUrl)
--------------------------------------------------------------------------------

data MiddlewareConfig = MiddlewareConfig { manager    :: Manager
                                         , middleware :: BaseUrl
                                         }
