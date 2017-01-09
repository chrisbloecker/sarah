{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Lens                 hiding ((#), element, set)
import Data.Text                           (unpack)
import Prelude                      hiding (div, span, id)
import Graphics.UI.Threepenny       hiding (map)
import Graphics.UI.Threepenny.Extra
import Sarah.Middleware.Model
--------------------------------------------------------------------------------

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


renderStatus :: Status -> [UI Element]
renderStatus status = map mkTile (status^.connectedNodes)
  where
    mkTile :: NodeInfo -> UI Element
    mkTile nodeInfo = div # set class_ "col-lg-4 col-md-4 col-sm-6"
                          #+ [ div # set class_ "panel panel-default"
                                   #+ [ div # set class_ "panel-heading"
                                            #+ [ div # set class_ "panel-title"
                                                     # set text (nodeInfo^.nodeName & unpack)
                                               ]
                                      , ul # set class_ "list-group"
                                           #+ map showDevice (nodeInfo^.nodeDevices)
                                      ]
                             ]

    showDevice :: Device -> UI Element
    showDevice device = li # set class_ "list-group-item"
                           #+ [ string (device^.deviceName      & unpack)
                              , string (device^.deviceModel     & show)
                              , string (device^.deviceInterface & show)
                              ]

renderRemotes :: [NodeInfo] -> [UI Element]
renderRemotes = concatMap renderNodeRemotes
  where
    renderNodeRemotes :: NodeInfo -> [UI Element]
    renderNodeRemotes node = map (renderRemote $ node^.nodeName) (node^.nodeDevices)

    renderRemote :: NodeName -> Device -> UI Element
    renderRemote nodeName device = case device^.deviceModel of
      AC _ -> do
        onButton <- button # set text "on"
        div #+ [ element onButton
               ]
      Sensor _ -> div
      TV _ -> div
