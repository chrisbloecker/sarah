{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Prelude                      hiding (div, span, id)
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Extra
--------------------------------------------------------------------------------

mkNavbar :: UI Element
mkNavbar =
  nav # set class_ "navbar navbar-default"
      #+ [ div # set class_ "container-fluid"
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
