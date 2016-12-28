{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Lens                 hiding ((#), set)
import Data.Text                           (unpack)
import Prelude                      hiding (div, span, id)
import Graphics.UI.Threepenny       hiding (map)
import Graphics.UI.Threepenny.Extra
import Sarah.Middleware.Model
--------------------------------------------------------------------------------

mkNavbar :: UI Element
mkNavbar =
  nav # set class_ "navbar navbar-default"
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
                                #+ [ li #+ [ a # set href "#"
                                               # set text "Home"
                                           ]
                                   , li # set class_ "dropdown"
                                        #+ [ a # set class_ "dropdown-toggle"
                                               # set datatoggle "dropdown"
                                               # set role "button"
                                               # set ariahaspopup "true"
                                               # set ariaexpanded "false"
                                               # set text "Rooms"
                                               # set href "#"
                                               #+ [ span # set class_ "caret"
                                                  ]
                                           , ul # set class_ "dropdown-menu"
                                                #+ [ li # set text "Livingroom"
                                                   , li # set text "Bedroom"
                                                   ]
                                           ]
                                   ]
                           ]
                  ]
         ]


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
