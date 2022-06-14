import Data.List
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

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

-- Used in myManageHook, `startsWith` query
startsWith :: Query String -> String -> Query Bool
startsWith q s = q >>= \t -> return (s `isPrefixOf` t)

myKeys :: [(String, X ())]
myKeys =
  [ -- Spawn a terminal instance.
    ("M-S-<Return>", spawn myTerminal),
    -- Spawn dmenu.
    ("M-p", spawn "dmenu_run"),
    -- Screenshot
    ("M-s", spawn "~/.local/bin/screenshot"),
    -- Toggle borders
    ("M-g", withFocused toggleBorder),
    -- Open Firefox
    ("M-f", spawn myBrowser),
    -- Multimedia Keys
    ("<XF86AudioPlay>", spawn "playerctl play-pause"),
    ("<XF86AudioPrev>", spawn "playerctl previous"),
    ("<XF86AudioNext>", spawn "playerctl next"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
    -- Close the focused window.
    ("M-S-c", kill),
    -- Layout Configuration
    -- Switch to the next layout.
    ("M-<Space>", sendMessage NextLayout),
    -- Reset the layout to the workspace default.
    -- ("M-S-<Space>", setLayout $ XMonad.layoutHook conf),
    -- Resize viewed windows to the correct size.
    ("M-n", refresh),
    -- Move focus to the next window.
    ("M-<Tab>", windows W.focusDown),
    -- Move focus to the next window.
    ("M-j", windows W.focusDown),
    -- Move focus to the previous window.
    ("M-k", windows W.focusUp),
    -- Move focus to the master window.
    ("M-m", windows W.focusMaster),
    -- Swap the focused window with the master window.
    ("M-<Return>", windows W.swapMaster),
    -- Swap the focused window with the next window.
    ("M-S-j", windows W.swapDown),
    -- Swap the focused window with the previous window.
    ("M-S-k", windows W.swapUp),
    -- Shrink the master area.
    ("M-h", sendMessage Shrink),
    -- Expand the master area.
    ("M-l", sendMessage Expand),
    -- Re-tile a window.
    ("M-t", withFocused $ windows . W.sink),
    -- Xmonad
    -- Quit Xmonad
    ("M-S-q", io exitSuccess),
    -- Restart Xmonad
    ("M-q", spawn "xmonad --recompile ; killall xmobar ; xmonad --restart")
  ]

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myStartupHook :: X ()
myStartupHook = do
  -- Cursor Styling
  setDefaultCursor xC_left_ptr
  -- Spawning daemons
  spawnOnce "picom --backend xrender &"
  spawnOnce "~/.fehbg"
  -- Setting the window manager name to LG3D to fix Java Swing apps
  setWMName "LG3D"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    [ -- Fullscreen
      isFullscreen --> doFullFloat,
      isFullscreen --> hasBorder False,
      -- GIMP
      className =? "Gimp" --> doFloat,
      -- Firefox
      (className =? "Firefox" <&&> title =? "File Upload") --> doFloat,
      (className =? "Firefox" <&&> title =? "File Upload") --> doFloat,
      (className `startsWith` "firefox" <&&> title =? "Close Firefox") --> doCenterFloat,
      -- Jetbrains
      (className =? "jetbrains-idea" <&&> title =? " ") --> doCenterFloat,
      title =? "Welcome to IntelliJ IDEA" --> doCenterFloat,
      className `startsWith` "jetbrains-" <&&> title =? "win0" --> doFloat,
      -- OBS
      className =? "obs" --> doFloat,
      -- Xmessage
      title =? "xmessage" --> doCenterFloat,
      -- Steam
      className =? "Steam" --> doCenterFloat,
      -- MultiMC
      className =? "MultiMC" --> doCenterFloat,
      -- Universal
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore,
      role =? "GtkFileChooserDialog" --> doCenterFloat,
      role =? "About" <||> role =? "about" --> doFloat,
      -- windown `startsWith` "Sign in" <||> windown `startsWith` "sign in" --> doFloat
      -- className =? "Navigator" --> doFloat
      windown `startsWith` "Sign in -" --> doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    windown = stringProperty "WM_NAME"

myEventHook = mempty

myGaps =
  spacingRaw
    False
    (Border 5 5 5 5)
    True
    (Border 5 5 5 5)
    True

myLayout = avoidStruts $ myGaps $ tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = xmobarColor "#61AFEF" "" . wrap "[" "]",
      ppHidden = xmobarColor "#ABB2BF" "",
      ppHiddenNoWindows = xmobarColor "#6b7089" "",
      ppSep = " > ",
      ppOrder = \(ws : l : t : ex) -> [ws] ++ map (xmobarColor "#E06C75" "") ex ++ [xmobarColor "#ABB2BF" "" t],
      ppExtras = []
    }

myConfig =
  def
    { modMask = myModMask,
      terminal = myTerminal,
      mouseBindings = myMouseBindings,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormColor,
      focusedBorderColor = myFocusColor,
      layoutHook = myLayout,
      startupHook = myStartupHook,
      manageHook = myManageHook,
      handleEventHook = myEventHook,
      workspaces = myWorkspaces
    }
    `additionalKeysP` myKeys

main :: IO ()
main =
  xmonad
    . docks
    . ewmhFullscreen
    . fullscreenSupport
    . ewmh
    . Hacks.javaHack
    . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobar/xmobar.hs" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
