{-# LANGUAGE 
   MultiWayIf   -- Required for `toggleFull` in `myAdditionalKeys`
   , LambdaCase -- Required for `(\case)` statement in `myXmobarPP`
   , FlexibleContexts
#-} 
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

-- Data Imports 
import qualified Data.Map                   as M
import           Data.Functor
import           Data.Monoid

-- Used in io exitSuccess 
import           System.Exit
import           System.Environment         (getEnv)
import           System.IO.Unsafe           (unsafeDupablePerformIO)

-- XMonad imports 
import           XMonad
import           XMonad.Actions.NoBorders   (toggleBorder)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders    
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet            as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig       (additionalKeysP)
import qualified XMonad.Util.Hacks          as Hacks
import           XMonad.Util.SpawnOnce

main :: IO ()
main = do
    xmonad
    . docks
    . ewmhFullscreen
    . fullscreenSupport
    . ewmh
    . Hacks.javaHack
    . withEasySB xmobar def
    $ myConfig

  -- Windows key/Super key
myModMask :: KeyMask
myModMask = mod4Mask 
  -- Default Terminal
myTerminal :: String
myTerminal = "alacritty" 
  -- Default Browser
myBrowser :: String
myBrowser = "firefox" 
  -- Workspaces
myWorkspaces :: [String]
myWorkspaces = map (wrap " " "" . show) [1 .. 9] 
  -- Border Width
myBorderWidth :: Dimension
myBorderWidth = 3 
  -- Formal Unfocused Color
myNormColor :: String
myNormColor = "#544862" 
  -- Focused Color
myFocusColor :: String
myFocusColor = "#61AFEF" 
  -- Home Directory
myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys = 
    base
        ++ window
        ++ applications
        ++ multimedia
    where 
    -- Force killing a frozen window.
        forceKillWindow :: Window -> X ()
        forceKillWindow w = withDisplay $ \d ->
            io $ void $ killClient d w
    -- Making a window have a full float over a workspace.
        toggleFull :: Window -> X () 
        toggleFull w = windows $ \s -> if 
            | M.lookup w (W.floating s) == Just fullscreen -> W.sink w s 
            | otherwise -> W.float w fullscreen s 
                where
                    fullscreen = W.RationalRect 0 0 1 1
    -- Screenshots
        screenShotSelection  = "screenshot -s" :: String 
        screenShotFullscreen = "screenshot -f" :: String
    -- XMonad base keybinds.
        base = 
            [ ("M-g",       withFocused toggleBorder)
            , ("M-S-c",     kill)
            , ("M-S-x",     withFocused forceKillWindow)
            , ("M-<Space>", sendMessage NextLayout)
            , ("M-n",       refresh)
            , ("M-S-q",     io exitSuccess)
            , ("M-q",       spawn "xmonad --recompile ; killall xmobar ; xmonad --restart")
            ]
    -- Window management keybinds.
        window = 
            [ ("M-<Tab>",    windows W.focusDown)
            , ("M-j",        windows W.focusDown)
            , ("M-k",        windows W.focusUp)
            , ("M-m",        windows W.focusMaster)
            , ("M-<Return>", windows W.swapMaster)
            , ("M-S-j",      windows W.swapDown)
            , ("M-S-k",      windows W.swapUp)
            , ("M-h",        sendMessage Shrink)
            , ("M-l",        sendMessage Expand)
            , ("M-t",        withFocused $ windows . W.sink)
            , ("M-S-f",      withFocused toggleFull)
            ]
    -- Spawning applications.
        applications =
            [ ("M-S-<Return>", spawn myTerminal)
            , ("M-f",          spawn myBrowser)
            , ("M-s",          spawn screenShotSelection)
            , ("<Print>",      spawn screenShotFullscreen)
            , ("M-p",          spawn "dmenu_run")
            ]
    -- Multimedia keybinds.
        multimedia =
            [ ("<XF86AudioPlay>",        spawn "playerctl play-pause")
            , ("<XF86AudioPrev>",        spawn "playerctl previous")
            , ("<XF86AudioNext>",        spawn "playerctl next")
            , ("<XF86AudioMute>",        spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
            , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
            , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
            , ("<Pause>",                spawn "amixer sset Capture toggle")
            ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  -- Set the window to floating mode and move by dragging.
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w      >> windows W.shiftMaster)
  -- Raise the window to the top of the stack.
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  -- Set the window to floating mode and resize by dragging.
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w    >> windows W.shiftMaster)
    ]

myStartupHook :: X ()
myStartupHook = do
    traverse spawnOnce
        [ "~/.fehbg"
        , "picom"
        , "dunst -conf $HOME/.config/dunst/dunstrc"
        ]
    setDefaultCursor xC_left_ptr
    setWMName "XMonad LG3D"

isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _)  = appName =? n

type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving Show

gimp    = ClassApp "Gimp"                  "gimp"
gimp2   = ClassApp "Gimp-2.99"             "gimp-2.99"
multimc = ClassApp "MultiMC"               "MultiMC"
about   = TitleApp "About Mozilla Firefox" "About Mozilla Firefox"
message = ClassApp "Xmessage"              "Xmessage"
steam   = ClassApp "Steam"                 "Steam"
obs     = ClassApp "obs"                   "obs"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = manageRules
    where
    -- Hides windows without ignoring it, see doHideIgnore in XMonad contrib.
        doHide = ask >>= doF . W.delete :: ManageHook
    -- WM_WINDOW_ROLE will be parsed with the role variable.
        isRole = stringProperty "WM_WINDOW_ROLE"
    -- To match multiple properties with one operator.
        anyOf = foldl (<||>) (pure False) :: [Query Bool] -> Query Bool
    -- To match multiple classNames with one operator.
        match = anyOf . fmap isInstance :: [App] -> Query Bool
    -- Checking for splash dialogs.
        isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
    -- Checking for pop-ups.
        isPopup = isRole =? "pop-up"
    -- Checking for file chooser dialog.
        isFileChooserDialog = isRole =? "GtkFileChooserDialog" 
    -- Managing rules for applications.
        manageRules = composeOne
            [ transience
            , isDialog     -?> doCenterFloat
            , isFullscreen -?> (doF W.focusDown <+> doFullFloat) 
            , match [ gimp
                    , gimp2
                    , about
                    , message
                    , obs
                    ]      -?> doFloat
                    , match [ steam
                    , multimc
                    ]      -?> doCenterFloat
                    , anyOf [ isFileChooserDialog
                    , isDialog
                    , isPopup
                    , isSplash
                    ]      -?> doCenterFloat
            ] <> composeAll
            [ manageDocks
            , className =? "firefox"    <&&> title    =? "File Upload" --> doFloat
            , className =? "firefox"    <&&> title    =? "Library"     --> doCenterFloat
            , className =? "firefox"    <&&> title    ^? "Save"        --> doFloat
            , className =? "firefox"    <&&> resource =? "Toolkit"     --> doFloat
            , className ^? "jetbrains-" <&&> title    ^? "Welcome to " --> doCenterFloat
            , className ^? "jetbrains-" <&&> title    =? "splash"      --> doFloat
            , className ^? "Visual "    <&&> isDialog                  --> doCenterFloat
            , resource  =? "desktop_window"                            --> doIgnore
            , resource  =? "kdesktop"                                  --> doIgnore
            , isRole    ^? "About"      <||> isRole   ^? "about"       --> doFloat
            , "_NET_WM_WINDOW_TYPE" `isInProperty` "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE" --> doIgnore <> doRaise
        -- Steam Game Fixes 
            , className =? "steam_app_1551360" <&&> title /=? "Forza Horizon 5" --> doHide -- Prevents black screen when fullscreening.
            , title     =? "Wine System Tray"                                   --> doHide -- Prevents Wine System Trays from taking input focus.
            , title     ^? "Steam - News"                                       --> doHide -- I don't like the Steam news menu 
            ]

{- May be useful one day 
doClose = ask >>= liftX . killWindow >> mempty :: ManageHook
doForceKill = ask >>= liftX . forceKillWindow >> mempty :: ManageHook
-}

myEventHook :: Event -> X All
myEventHook _ = return (All True)

myLayoutHook =
    avoidStruts
    $ lessBorders OnlyScreenFloat
    $ spacingRaw False(Border w w w w) True(Border w w w w) True
    $ tiled ||| Mirror tiled ||| Full
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1     -- Default number of windows in the master pane.
        ratio = 1 / 2   -- Default proportion of screen occupied by master panes.
        delta = 3 / 100 -- Percent of screen increment by when resizing panes.
        w = 7           -- Width of pixel size between windows while tiled. 

myXmobarPP :: X PP
myXmobarPP =
    clickablePP $ def
        { ppCurrent          = xmobarColor "#61AFEF" ""
        , ppVisibleNoWindows = Just (xmobarColor "#A9B1D6" "")
        , ppHidden           = xmobarColor "#ABB2BF" ""
        , ppHiddenNoWindows  = xmobarColor "#6B7089" ""
        , ppUrgent           = xmobarColor "#F7768E" "" . wrap "!" "!"
        , ppTitle            = xmobarColor "#CCD0D8" "" . shorten 49 
        , ppSep              = " "
        , ppWsSep            = xmobarColor "" "#282C34:5" "  "
        , ppLayout           = xmobarColor "#31353F" "" 
                               . (\case
                                   "Spacing Tall"        -> "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn><icon=tiled.xpm/><fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn>"
                                   "Spacing Mirror Tall" -> "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn><icon=mirrortiled.xpm/><fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn>"
                                   "Spacing Full"        -> "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn><icon=full.xpm/><fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn>"
                                 )                     
        }

xmobar :: StatusBarConfig
xmobar = statusBarProp myXmobar myXmobarPP 

myXmobar :: String
myXmobar = ("xmobar " ++ myHomeDir ++ "/.config/xmonad/src/xmobar.hs")

myConfig = 
    def
        { modMask            = myModMask
        , terminal           = myTerminal
        , mouseBindings      = myMouseBindings
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , layoutHook         = myLayoutHook
        , startupHook        = myStartupHook
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook
        , workspaces         = myWorkspaces
        } `additionalKeysP` myAdditionalKeys
