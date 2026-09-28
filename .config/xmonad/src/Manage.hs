module Manage
    ( myManageHook
    , myLateManageHook
    ) where

import           Scratchpads                 (myScratchpads)
import           Screens                     (otherScreen)
import           XMonad
import           XMonad.Hooks.ManageDocks    (manageDocks)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders     (hasBorder)
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

floatApps, centerFloatApps, secondaryMonitorApps, hideApps :: [Query Bool]
floatApps =
       map (className =?) ["Gimp", "Xmessage", "obs"]
    ++ map (title     =?) ["About LibreWolf", "Sign In", "Toolkit", "File Upload", "Save"]

centerFloatApps =
       map (className =?) ["PrismLauncher", "steam", "Blueman-services", "Blueman-manager"]
    ++ map (title     =?) [ "Library", "Remove methods", "Remove fields", "Rename class"
                          , "Select a destination package", "Remove annotations" ]

secondaryMonitorApps =
    map (className    =?) ["vesktop", "Spotify", "signal"]

hideApps =
    map (title        =?) ["Wine System Tray", "Steam - News"]

identityRules :: [MaybeManageHook]
identityRules =
    [ anyOf floatApps            -?> doFloat
    , anyOf hideApps             -?> doHideIgnore
    , anyOf centerFloatApps      -?> doCenterFloat
    , anyOf secondaryMonitorApps -?> doSendToOtherScreen
    ]

identityRulesAll :: [ManageHook]
identityRulesAll =
    [ className ^? "jetbrains-"     <&&> title ^? "Welcome to " --> doCenterFloat
    , className ^? "jetbrains-"     <&&> title ^? "splash"      --> (doFloat <+> hasBorder False)
    , className ^? "jetbrains-"     <&&> title ^? "win"         --> hasBorder False
    , className ^? "software.coley" <&&> title =? ""            --> doCenterFloat
    , className ^? "software.coley" <&&> title =? "Config"      --> doCenterFloat
    ]

kindRules :: [MaybeManageHook]
kindRules =
    [ anyOf [ isDialog
            , isRole =? "pop-up"
            , isRole =? "Popup"
            , isRole =? "GtkFileChooserDialog"
            , isSplash
            , title =? "System information"
            ]                    -?> doCenterFloat
    , isRole =? "pop-up" <||> isRole =? "Popup" -?> hasBorder False
    ]

myManageHook :: ManageHook
myManageHook =
       composeOne (identityRules <> kindRules)
    <> composeAll (transience' : manageDocks : identityRulesAll)
    <> namedScratchpadManageHook myScratchpads

myLateManageHook :: ManageHook
myLateManageHook = composeOne identityRules <> composeAll identityRulesAll

anyOf :: [Query Bool] -> Query Bool
anyOf = foldr (<||>) (pure False)

isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

doSendToOtherScreen :: ManageHook
doSendToOtherScreen = do
    mws <- liftX $ otherScreen >>= maybe (pure Nothing) screenWorkspace
    w   <- ask
    doF $ maybe id (`W.shiftWin` w) mws