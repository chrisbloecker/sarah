{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Lens                 hiding ((#), element, set)
import Control.Monad                       (forM)
import Control.Monad.Reader                (ask)
import Data.Text                           (unpack)
import Prelude                      hiding (div, span, id)
import Graphics.UI.Threepenny       hiding (map)
import Graphics.UI.Threepenny.Extra
import Sarah.GUI.Model                     (AppEnv, AppIO, manager, middleware)
import Sarah.Middleware.Model       hiding (manager)
--------------------------------------------------------------------------------
import           Sarah.Middleware.Device.AC.Toshiba as AC
import qualified Sarah.Middleware.Client as Middleware
--------------------------------------------------------------------------------

pam = flip map

mkNavbar :: UI (Element, Element, Element)
mkNavbar = do
  remotesLink <- a # set href "#"
                   # set text "Remotes"
  devicesLink <- a # set href "#"
                   # set text "Devices"
  navbar <- nav # set class_ "navbar navbar-default"
                #+ [ div # set class_ "container"
                         #+ [ div # set class_ "navbar-header"
                                  #+ [ button # set type_ "button"
                                              # set class_ "navbar-toggle collapsed"
                                              # set datatoggle "collapse"
                                              # set datatarget "#the-navbar"
                                              # set ariaexpanded "false"
                                              #+ [ span # set class_ "icon-bar"
                                                 , span # set class_ "icon-bar"
                                                 , span # set class_ "icon-bar"
                                                 ]
                                     ]
                            , div # set class_ "collapse navbar-collapse"
                                  # set id_ "the-navbar"
                                  #+ [ ul # set class_ "nav navbar-nav"
                                          #+ [ li #+ [ element remotesLink
                                                     ]
                                             , li #+ [ element devicesLink
                                                     ]
                                             ]
                                     ]
                            ]
                   ]
  return (remotesLink, devicesLink, navbar)


mkTile :: String -> UI Element -> UI Element
mkTile title content = div # set class_ "col-lg-4 col-md-4 col-sm-6"
                           #+ [ div # set class_ "panel panel-default"
                                    #+ [ div # set class_ "panel-heading"
                                             #+ [ div # set class_ "panel-title"
                                                      # set text title
                                                ]
                                       , content
                                       ]
                              ]

listGroup :: String -> [UI Element] -> UI Element
listGroup title contents = ul # set class_ "list-group"
                              #+ [ li # set class_ "list-group-item"
                                      #+ [ span # set class_ "glyphicon glyphicon-chevron-right"
                                                # set text title
                                         , ul # set class_ "list-group"
                                              #+ contents
                                         ]
                                 ]

listGroupItem :: [UI Element] -> UI Element
listGroupItem es = li # set class_ "list-group-item"
                      #+ es


renderStatus :: Status -> [UI Element]
renderStatus status = map renderNodeInfo (status^.connectedNodes) -- $ \nodeInfo -> mkTile (nodeInfo^.nodeName & unpack) (nodeInfo^.nodeDevices & map )
  where
    renderNodeInfo :: NodeInfo -> UI Element
    renderNodeInfo nodeInfo = mkTile (nodeInfo^.nodeName & unpack) (listGroup "Devices" $ map showDevice (nodeInfo^.nodeDevices))

    showDevice :: Device -> UI Element
    showDevice device = listGroupItem [ string (device^.deviceName      & unpack)
                                      , string (device^.deviceModel     & show)
                                      ]


renderRemotes :: AppEnv -> [NodeInfo] -> [UI Element]
renderRemotes appEnv = concatMap (renderNodeRemotes appEnv)
  where
    renderNodeRemotes :: AppEnv -> NodeInfo -> [UI Element]
    renderNodeRemotes appEnv node = map (renderRemote appEnv $ node^.nodeName) (node^.nodeDevices)

    renderRemote :: AppEnv -> NodeName -> Device -> UI Element
    renderRemote appEnv nodeName device =
      let title = unpack nodeName ++ " " ++ unpack (device^.deviceName)
      in mkTile title $ case device^.deviceModel of
                          AC _ -> do
                            onButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-flash" ]
                            offButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-off" ]
                            coolButton <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "fa fa-snowflake-o" ]
                            dryButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-tint" ]
                            fanButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-cloud" ]
                            ecoButton  <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-leaf" ]
                            hiButton   <- button # set class_ "btn btn-sm btn-default" #+ [ span # set class_ "glyphicon glyphicon-fire" ]


                            -- ToDo: get the state of the device and modify it, don't just overwrite the state
                            on click onButton   $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
                            on click offButton  $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeOff  Nothing)             (appEnv^.manager) (appEnv^.middleware)
                            on click coolButton $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool Nothing)             (appEnv^.manager) (appEnv^.middleware)
                            on click dryButton  $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeDry  Nothing)             (appEnv^.manager) (appEnv^.middleware)
                            on click fanButton  $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeFan  Nothing)             (appEnv^.manager) (appEnv^.middleware)
                            on click ecoButton  $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeAuto (Just AC.PowerEco))  (appEnv^.manager) (appEnv^.middleware)
                            on click hiButton   $ \_ -> runEIO $ Middleware.runAcServer (AC.Config AC.T22 AC.FanAuto AC.ModeCool (Just AC.PowerHigh)) (appEnv^.manager) (appEnv^.middleware)

                            div #+ [ p # set class_ "text-center"
                                       #+ map element[ onButton, offButton ]
                                   , p # set class_ "text-center"
                                       #+ map element [ coolButton, dryButton, fanButton ]
                                   , p # set class_ "text-center"
                                       #+ map element [ ecoButton, hiButton ]
                                   ]
                          -- ToDo: build those remotes somewhere else
                          Sensor _ -> div
                          TV _ -> div
