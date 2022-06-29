import qualified Data.Map                   as M
import 		 Data.List
import 		 Data.Monoid
import 		 System.Exit
import 		 XMonad
import 		 XMonad.Actions.NoBorders
import 		 XMonad.Hooks.EwmhDesktops
import 		 XMonad.Hooks.ManageDocks
import 		 XMonad.Hooks.ManageHelpers
import  	 XMonad.Hooks.SetWMName
import 		 XMonad.Hooks.StatusBar
import 		 XMonad.Hooks.StatusBar.PP
import 		 XMonad.Layout.Fullscreen
import 		 XMonad.Layout.NoBorders
import 		 XMonad.Layout.Spacing
import qualified XMonad.StackSet            as W
import  	 XMonad.Util.ClickableWorkspaces
import 		 XMonad.Util.Cursor
import 		 XMonad.Util.EZConfig	    (additionalKeysP)
import qualified XMonad.Util.Hacks          as Hacks
import 		 XMonad.Util.SpawnOnce

myModMask :: KeyMask
myModMask = mod4Mask      -- Windows key/Super key

myTerminal :: String
myTerminal = "alacritty"  -- Default Terminal

myBrowser :: String
myBrowser = "firefox"     -- Default Browser

myWorkspaces :: [WorkspaceId]
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

-- Colors
myNormColor :: String
myNormColor = "#544862"

myFocusColor :: String
myFocusColor = "#61AFEF"

-- Border Width
myBorderWidth :: Dimension
myBorderWidth = 3

myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
   [ ("M-g",        withFocused toggleBorder)
   , ("M-S-c",      kill)
   , ("M-S-x",	    forceKill)
   , ("M-<Space>",  sendMessage NextLayout)
   , ("M-n", 	    refresh)
  -- Windows
   , ("M-<Tab>",    windows W.focusDown)
   , ("M-j", 	    windows W.focusDown)
   , ("M-k", 	    windows W.focusUp)
   , ("M-m", 	    windows W.focusMaster)
   , ("M-<Return>", windows W.swapMaster)
   , ("M-S-j", 	    windows W.swapDown)
   , ("M-S-k", 	    windows W.swapUp)
   , ("M-h",        sendMessage Shrink)
   , ("M-l",        sendMessage Expand)
   , ("M-t",        withFocused $ windows . W.sink)
   , ("M-S-f",	    toggleFull)
  -- Quit
   , ("M-S-q", 	    io exitSuccess)
   , ("M-q",   	    spawn "xmonad --recompile ; killall xmobar ; xmonad --restart")
  -- Applications
   , ("M-S-<Return>", spawn myTerminal)
   , ("M-f",	      spawn myBrowser)
   , ("M-s", 	      spawn "screenshot")
  -- Dmenu
   , ("M-p", 	      spawn "dmenu_run")
  -- Multimedia Keys
   , ("<XF86AudioPlay>",	spawn "playerctl play-pause")
   , ("<XF86AudioPrev>", 	spawn "playerctl previous")
   , ("<XF86AudioNext>", 	spawn "playerctl next")
   , ("<XF86AudioMute>", 	spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
   , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%")
   , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%")
   ]
   where
  -- Making a window have a full float over a workspace.
    toggleFull = withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1)
  -- Force killing a frozen window.
    forceKillWindow w = withDisplay $ \d -> 
      io $ killClient d w >> return () 
    forceKill = withFocused forceKillWindow :: X ()


myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  -- Set the window to floating mode and move by dragging.
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w 	>> windows W.shiftMaster)
  -- Raise the window to the top of the stack.
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  -- Set the window to floating mode and resize by dragging.
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w 	>> windows W.shiftMaster)
    ]

myStartupHook :: X ()
myStartupHook = do
  -- Cursor Styling
    setDefaultCursor xC_left_ptr
  -- Spawning daemons
    spawnOnce "picom &"
    spawnOnce "~/.fehbg"
  -- Setting the window manager name to LG3D to fix Java Swing apps
    setWMName "XMonad LG3D"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  -- Fullscreen
    [ isFullscreen --> doFullFloat
  -- GIMP
    , className  =? "Gimp" --> doFloat
  -- Firefox
    , className =? "firefox"	       <&&> title =? "File Upload"   --> doFloat
    , className `startsWith` "firefox" <&&> title =? "Close Firefox" --> doCenterFloat
  -- Jetbrains
    , className `startsWith` "jetbrains-" <&&> title `startsWith` "Welcome to " --> doCenterFloat
    , className `startsWith` "jetbrains-" <&&> title =? "splash"                --> doFloat
  -- OBS
    , className =? "obs"      --> doFloat
  -- X/X11
    , title     =? "xmessage" --> doCenterFloat
  -- Steam
    , className =? "Steam"    	      --> doCenterFloat
    , title	=? "Wine System Tray" --> doHide 
  -- MultiMC
    , className =? "MultiMC"  --> doCenterFloat
  -- Universal
    , resource =? "desktop_window"         --> doIgnore
    , resource =? "kdesktop"               --> doIgnore
    , role =? "GtkFileChooserDialog"       --> doCenterFloat
    , role =? "About" <||> role =? "about" --> doFloat
    , isDialog 				   --> doFloat
    ]
    where
  -- Hides windows from appearing in a workspace.
      doHide = ask >>= doF . W.delete :: ManageHook
  -- Checks if a string of text "starts with" given text.
      startsWith q s = isPrefixOf s <$> q
  -- Checking the name of a role and icon.
      role = stringProperty "WM_WINDOW_ROLE"

--    doClose = ask >>= liftX . killWindow >> mempty :: ManageHook
--    doForceKill = ask >>= liftX . forceKillWindow >> mempty :: ManageHook

myEventHook :: Event -> X All
myEventHook = mempty

myGaps = spacingRaw False(Border w w w w) True(Border w w w w) True
    where
      w = 5

myLayout =
    avoidStruts
    $ lessBorders OnlyScreenFloat
    $ myGaps 
    $ tiled ||| Mirror tiled ||| Full
    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio = 1 / 2
      delta = 3 / 100

myXmobarPP :: X PP
myXmobarPP = clickablePP $ def
    { ppCurrent 	 = xmobarColor "#61AFEF" "" . wrap "<box type=Bottom offset=C5 width=3 color=#61AFEF>" "</box>" 
    , ppHidden 		 = xmobarColor "#ABB2BF" ""
    , ppVisibleNoWindows = Just (xmobarColor "#a9b1d6" "")
    , ppHiddenNoWindows  = xmobarColor "#6b7089" ""
    , ppUrgent 		 = xmobarColor "#F7768E" "" . wrap "!" "!"
    , ppSep 		 = "<fc=#E06C75> > </fc> "
    , ppTitle 		 = xmobarColor "#CCD0D8" "" . shorten 90
    , ppOrder      	 = \(ws : l : t : ex) -> [ws] ++ map (xmobarColor "#E06C75" "") ex ++ [xmobarColor "#ABB2BF" "" t]
    , ppExtras 	   	 = []
    }

xmobar :: StatusBarConfig
xmobar = statusBarProp "xmobar ~/.config/xmonad/xmobar/xmobar.hs" myXmobarPP

myConfig = def
    { modMask            = myModMask
    , terminal           = myTerminal
    , mouseBindings      = myMouseBindings
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , layoutHook         = myLayout
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , workspaces         = myWorkspaces
    } `additionalKeysP` myKeys

main :: IO ()
main = do
    xmonad
    . docks
    . ewmhFullscreen
    . fullscreenSupport
    . ewmh
    . Hacks.javaHack
    . withSB xmobar
    $ myConfig
