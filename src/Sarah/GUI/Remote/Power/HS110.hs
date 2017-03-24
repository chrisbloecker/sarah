{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Remote.Power.HS110
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader           (runReaderT, lift, ask)
import Graphics.UI.Bootstrap
import Graphics.UI.Threepenny  hiding (map)
import Prelude                 hiding (div)
import Sarah.GUI.Model
import Sarah.GUI.Websocket            (withoutResponse)
import Sarah.Middleware.Device        (HS110)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Device.Power.HS110 as HS110
--------------------------------------------------------------------------------

instance HasRemote HS110 where
  buildRemote _ = do
    RemoteBuilderEnv{..} <- ask
    lift $ do
      let whiteButton = buildClass [ btn, btn_sm, btn_default, btn_no_background ]

      onButton  <- button # set class_ (unClass whiteButton) #+ [ label # set text "On"  ]
      offButton <- button # set class_ (unClass whiteButton) #+ [ label # set text "Off" ]

      on click onButton  $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse HS110.PowerOn
      on click offButton $ embedUI $ flip runReaderT remoteRunnerEnv $ withoutResponse HS110.PowerOff

      div #+ [ div # set class_ "row text-center"
                   #+ map element [ onButton, offButton ]
             ]
