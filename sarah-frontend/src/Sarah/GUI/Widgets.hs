{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Sarah.GUI.Widgets
  where
--------------------------------------------------------------------------------
import Control.Lens                 hiding ((#), element, set)
import Control.Monad                       (forM)
import Control.Monad.Reader                (ask)
import Data.Either                         (isRight)
import Data.Either.Combinators             (fromRight)
import Data.Text                           (unpack)
import Prelude                      hiding (div, span, id)
import Graphics.UI.Threepenny       hiding (map)
import Graphics.UI.Threepenny.Extra
import Sarah.GUI.Model                     (AppEnv, AppIO, manager, middleware)
import Sarah.Middleware.Model       hiding (manager)
import Sarah.Middleware
import Sarah.GUI.Remote
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
    renderNodeInfo nodeInfo = mkTile (nodeInfo^.nodeName & unpack) (listGroup "Devices" $ map (uncurry showDevice) (nodeInfo^.nodeDevices))

    showDevice :: DeviceName -> DeviceRep -> UI Element
    showDevice deviceName deviceRep = listGroupItem [ string ("a device: " ++ unpack deviceName) ]


renderRemotes :: AppEnv -> [NodeInfo] -> [UI Element]
renderRemotes appEnv = concatMap (renderNodeRemotes appEnv)
  where
    renderNodeRemotes :: AppEnv -> NodeInfo -> [UI Element]
    renderNodeRemotes appEnv node = catRight . flip map (node^.nodeDevices) $ \(deviceName, deviceRep) ->
      -- because of the existential, we have to pattern match here in order to get the model
      case fromDeviceRep deviceRep of
        Left err             -> Left err
        Right (Remote model) -> let widget = renderRemote appEnv (DeviceAddress (node^.nodeName) deviceName) model
                                in Right $ mkTile (unpack deviceName) widget

    catRight :: [Either l r] -> [r]
    catRight = let cat = \case Left  l -> error "catRight" -- this is really not supposed to happen
                               Right r -> r
               in map cat . filter isRight
