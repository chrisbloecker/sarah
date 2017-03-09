{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Sarah.GUI
  ( setup
  ) where
--------------------------------------------------------------------------------
import Control.Monad                             (void)
import Control.Monad.Reader                      (runReaderT)
import Data.Text                                 (unpack)
import Data.Either                               (isRight)
import Graphics.UI.Threepenny             hiding (map)
import Graphics.UI.Threepenny.Extra
import Prelude                            hiding (div, span)
import Sarah.GUI.Model
import Sarah.GUI.Remote                          (Remote (..), fromDeviceRep)
import Sarah.GUI.Widgets
import Sarah.Middleware                          (DeviceAddress (..), DeviceName, DeviceRep, Status (..), NodeInfo (..), runEIO)
--------------------------------------------------------------------------------
import qualified Sarah.Middleware.Client as Middleware
--------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup appEnv@AppEnv{..} window = void $ do
  (remotesLink, devicesLink, navbar)  <- mkNavbar

  on click remotesLink $ \_ -> do
    mapM_ delete =<< getElementById window "content"
    devices <- runEIO $ Middleware.getStatus manager middleware
    getBody window #+ [ div # set id_ "content"
                            # set class_ "container"
                            #+ either (const []) (\Status{..} -> renderRemotes appEnv connectedNodes) devices
                      ]

  on click devicesLink $ \_ -> do
    mapM_ delete =<< getElementById window "content"
    status <- runEIO $ Middleware.getStatus manager middleware
    getBody window #+ [ div # set id_ "content"
                            # set class_ "container"
                            #+ either (const []) renderStatus status
                      ]

  getBody window #+ [ element navbar
                    , div # set id_ "content"
                    ]

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
renderStatus Status{..} = map renderNodeInfo connectedNodes -- $ \nodeInfo -> mkTile (nodeInfo^.nodeName & unpack) (nodeInfo^.nodeDevices & map )
  where
    renderNodeInfo :: NodeInfo -> UI Element
    renderNodeInfo NodeInfo{..} = mkTile (unpack nodeName) (listGroup "Devices" $ map (uncurry showDevice) nodeDevices)

    showDevice :: DeviceName -> DeviceRep -> UI Element
    showDevice deviceName deviceRep = listGroupItem [ string ("a device: " ++ unpack deviceName) ]


renderRemotes :: AppEnv -> [NodeInfo] -> [UI Element]
renderRemotes appEnv = concatMap (renderNodeRemotes appEnv)
  where
    renderNodeRemotes :: AppEnv -> NodeInfo -> [UI Element]
    renderNodeRemotes appEnv NodeInfo{..} = catRight . flip map nodeDevices $ \(deviceName, deviceRep) ->
      -- because of the existential, we have to pattern match here in order to get the model
      case fromDeviceRep deviceRep of
        Left err             -> Left err
        Right (Remote model) -> let deviceAddress = DeviceAddress nodeName deviceName
                                    widget = runReaderT (buildRemote model) RemoteBuilderEnv{..}
                                in Right $ mkTile (unpack deviceName) widget

    catRight :: [Either l r] -> [r]
    catRight = let cat = \case Left  l -> error "catRight" -- this is really not supposed to happen
                               Right r -> r
               in map cat . filter isRight
