{-# LANGUAGE MultiWayIf, LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans  #-}

{- Data imports -} 
import qualified Data.Map                            as M
import           Data.Functor                        (void)
import           Data.Foldable                       (traverse_)
import           Data.Monoid

{- System imports -} 
import           System.Exit
import           System.Environment                  (getEnv)
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

{- Local imports -}
import           Theme.Theme

{- XMonad imports -} 
import           XMonad
import           XMonad.Actions.NoBorders            (toggleBorder)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.NoBorders    
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet                     as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig                (additionalKeysP)
import qualified XMonad.Util.Hacks                   as Hacks
import           XMonad.Util.NamedScratchpad         (NamedScratchpad (NS),
                                                      customFloating,
						      namedScratchpadAction,
						      namedScratchpadManageHook,
						      scratchpadWorkspaceTag)
import           XMonad.Util.SpawnOnce

-- Windows key/Super key
myModMask :: KeyMask
myModMask = mod4Mask 

-- Default Terminal
myTerminal :: String
myTerminal = "alacritty"

-- Default Browser
myBrowser :: String
myBrowser = "librewolf" 

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = map show ([1..9] :: [Int])

-- Border Width
myBorderWidth :: Dimension
myBorderWidth = 2 

-- Formal Unfocused Color
myNormColor :: String
myNormColor = "#544862" 

-- Focused Color
myFocusColor :: String
myFocusColor = base04 

-- Home Directory
myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

red, blue, magenta, white :: String 
red        = base01
blue       = base04
magenta    = base05
magenta1   = base0D
white      = base07 
background = base08 <> ":5"

main :: IO ()
main = do
    xmonad
    . docks
    . ewmhFullscreen
    . ewmh
    . Hacks.javaHack
    . withEasySB (statusBarProp myXmobar myXmobarPP) def 
    $ myConfig
  where
    myXmobar = "xmobar -x 0 " ++ myHomeDir ++ "/.config/xmonad/src/xmobar.hs"

myConfig = def
    { modMask            = myModMask
    , terminal           = myTerminal
    , mouseBindings      = myMouseBindings
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , handleEventHook    = \_ -> return (All True) 
    , workspaces         = myWorkspaces
    } `additionalKeysP`    myKeys

myKeys :: [(String, X ())]
myKeys = concat 
    [
    -- Base keybindings
      [ ("M-g",             withFocused toggleBorder)
      , ("M-S-c",           kill)
      , ("M-S-x",           withFocused $ \w -> withDisplay $ \d -> io $ void $ killClient d w)
      , ("M-<Space>",       sendMessage NextLayout)
      , ("M-n",             refresh)
      , ("M-S-q",           myQuitHook)
      , ("M-q",             spawn "xmonad --recompile ; killall xmobar ; xmonad --restart")
      ]
    -- Window management keybindings
    , [ ("M-<Tab>",         windows W.focusDown)
      , ("M-j",             windows W.focusDown)
      , ("M-k",             windows W.focusUp)
      , ("M-m",             windows W.focusMaster)
      , ("M-<Return>",      windows W.swapMaster)
      , ("M-S-j",           windows W.swapDown)
      , ("M-S-k",           windows W.swapUp)
      , ("M-h",             sendMessage Shrink)
      , ("M-l",             sendMessage Expand)
      , ("M-t",             withFocused $ windows . W.sink)
      , ("M-S-f",           withFocused toggleFull)
      ]
    -- Scratchpad keybindings
    , [ ("M-C-<Return>",    namedScratchpadAction myScratchpads "terminal")
      , ("M-C-<Backspace>", namedScratchpadAction myScratchpads "htop")
      ]
    , [ ("M-S-<Return>",    spawn myTerminal)
      , ("M-f",             spawn myBrowser)
      , ("M-s",             spawn "screenshot -s")
      , ("<Print>",         spawn "screenshot -f")
      , ("M-p",             rofiCmd "")
      ]
    , multimediaKeys
    ]
  where
    toggleFull w = windows $ \s ->
        if M.lookup w (W.floating s) == Just (W.RationalRect 0 0 1 1)
	    then W.sink w s
	    else W.float w (W.RationalRect 0 0 1 1) s

    rofiCmd cmd = do
        screen <- gets (W.screen . W.current . windowset)
	spawn $ "rofi -show run -monitor " ++ show (fromIntegral screen + 1 :: Int) ++ " " ++ cmd

multimediaKeys :: [(String, X ())]
multimediaKeys = 
    [ ("<XF86AudioPlay>",   spawn "playerctl play-pause")
    , ("<Pause>",           spawn "playerctl play-pause")
    , ("M-<Left>",          spawn "playerctl previous")
    , ("M-<Right>",         spawn "playerctl next")
    , ("<XF86AudioPrev>",   spawn "playerctl previous")
    , ("<XF86AudioNext>",   spawn "playerctl next")
    ] ++ volumeKeys
  where
    volumeKeys = 
        [ (k, spawn $ "wpctl set-" ++ cmd ++ " @DEFAULT_AUDIO_SINK@ " ++ arg)
        | (k, cmd, arg) <- [ ("M-<Down>",               "volume", "5%-")
                           , ("M-<Up>",                 "volume", "5%+")
                           , ("<XF86AudioMute>",        "mute",   "toggle")
                           , ("<XF86AudioLowerVolume>", "volume", "2%-")
                           , ("<XF86AudioRaiseVolume>", "volume", "2%+")
                           ]
        ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    {- Set the window to floating mode and move by dragging. -}
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w      >> windows W.shiftMaster)
    {- Raise the window to the top of the stack. -}
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    {- Set the window to floating mode and resize by dragging. -}
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w    >> windows W.shiftMaster)
    ]

myStartupHook :: X ()
myStartupHook = do
    _ <- traverse spawnOnce
        [ "picom &"
        , "dunst -conf " ++ myHomeDir ++ "/.config/dunst/dunstrc"
        , "gentoo-pipewire-launcher &"
        , myHomeDir ++ "/.fehbg"
        ]
    screenWorkspace 1 >>= flip whenJust (windows . W.view)
    {- Forcing the second screen to be our second workspace, 
     - it is labeled as 1 because our map starts at 0 -}
    windows $ W.greedyView (myWorkspaces !! 1)
    {- Re-focusing our original screen, 0. 
     - This ensures that we will always have our 1st 
     - workspace on our primary monitor. -}
    screenWorkspace 0 >>= flip whenJust (windows . W.view)
    setDefaultCursor xC_left_ptr

myQuitHook :: X ()
myQuitHook = do
    traverse_ (spawnOnce . ("killall " ++)) ["picom", "pipewire", "wireplumber"]
    io exitSuccess

myScratchpads :: [NamedScratchpad]
myScratchpads = 
    [ NS "terminal" (myTerminal ++ " --class Scratchpad") (className =? "Scratchpad") floatConf
    , NS "htop"     (myTerminal ++ " --class HTOP -e htop") (className =? "HTOP") floatConf
    ]
  where floatConf = customFloating $ W.RationalRect (1/6) (1/8) (2/3) (3/4)

-- App definitions using a record
data App = App { appQuery :: Query Bool }

mkApp :: String -> App
mkApp cls = App (className =? cls)

mkTitle :: String -> App  
mkTitle ttl = App (title =? ttl)

-- App lists
floatApps :: [App]
floatApps = map mkApp 
    [ "Gimp"
    , "Xmessage"
    , "obs"
    ] ++ map mkTitle
    [ "About LibreWolf"
    , "Sign In"
    , "Toolkit"
    , "File Upload"
    , "Save"
    ]

centerFloatApps :: [App]
centerFloatApps = map mkApp
    [ "PrismLauncher"
    , "steam"
    , "Blueman-services"
    , "Blueman-manager"
    ] ++ map mkTitle
    [ "Library"
    , "Remove methods"
    , "Remove fields"
    , "Rename class"
    , "Select a destination package"
    , "Remove annotations"
    ]

secondaryMonitorApps :: [App]
secondaryMonitorApps = map mkApp
    [ "vesktop"
    , "Spotify"
    , "Signal"
    ]

hideApps :: [App]
hideApps = [mkTitle "Wine System Tray", mkTitle "Steam - News"]

myManageHook :: ManageHook
myManageHook = composeOne
    [ transience
    , match floatApps            -?> doFloat
    , match centerFloatApps      -?> doCenterFloat
    , match hideApps             -?> doHide
    , match secondaryMonitorApps -?> doSendToScreen (S 1) 
    , anyOf 
        [ isDialog
	, isRole =? "pop-up"
	, isRole =? "Popup"
	, isRole =? "GtkFileChooserDialog"
	, isSplash
	, title =? "System information"
	] -?> doCenterFloat
    , isRole =? "pop-up" <||> isRole =? "Popup" -?> hasBorder False
    ] <> composeAll
    [ manageDocks
    , className ^? "jetbrains-"     <&&> title ^? "Welcome to " --> doCenterFloat
    , className ^? "jetbrains-"     <&&> title ^? "splash"      --> (doFloat <+> hasBorder False)
    , className ^? "jetbrains-"     <&&> title ^? "win"         --> hasBorder False
    , className ^? "software.coley" <&&> title =? ""            --> doCenterFloat
    , className ^? "software.coley" <&&> title =? "Config"      --> doCenterFloat
    ] <+> namedScratchpadManageHook myScratchpads
  where
    match    = anyOf . map appQuery
    anyOf    = foldl (<||>) (pure False)
    isRole   = stringProperty "WM_WINDOW_ROLE"
    isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
    doHide   = ask >>= doF . W.delete

-- Force a window to always appear on the same ScreenId
doSendToScreen :: ScreenId -> ManageHook
doSendToScreen sid = ask >>= (\w -> doF . shifter w =<< maybeWs)
  where maybeWs = liftX $ screenWorkspace sid
        shifter win (Just ws) = W.shiftWin ws win
        shifter _   Nothing   = id

myLayoutHook =
    avoidStruts
    $ lessBorders OnlyScreenFloat
    $ spacingRaw False(Border w w w w) True(Border w w w w) True
    $ tiled ||| Mirror tiled ||| Full 
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1       {- Default number of windows in the master pane.          -}
        ratio   = 1 / 2   {- Default proportion of screen occupied by master panes. -}
        delta   = 3 / 100 {- Percent of screen increment by when resizing panes.    -}
        w       = 7       {- Width of pixel size between windows while tiled.       -} 

myXmobarPP :: X PP
myXmobarPP = clickablePP $ filterOutWsPP [scratchpadWorkspaceTag] myPP

myPP :: PP
myPP = def
    { ppCurrent          = colorFont red 4
    , ppVisibleNoWindows = Just $ colorize magenta
    , ppVisible          = colorize blue
    , ppHidden           = colorize white
    , ppHiddenNoWindows  = colorize magenta1
    , ppUrgent           = colorFont red 1 . wrap "!" "!"
    , ppTitle            = colorize white . shorten 80
    , ppSep              = wrapSep " "
    , ppTitleSanitize    = xmobarStrip
    , ppWsSep            = xmobarColor "" background "   "
    , ppLayout           = layoutIcon
    }
  where
    colorize :: String -> String -> String
    colorize color = xmobarColor color background
    
    colorFont :: String -> Int -> String -> String
    colorFont color font = xmobarColor color background . xmobarFont font
    
    wrapSep :: String -> String
    wrapSep = wrap powerlineLeft powerlineRight
      where
        powerlineLeft  = xmobarColor background (basebg <> ":6") (xmobarFont 2 "\xe0b4")
        powerlineRight = xmobarColor background (basebg <> ":6") (xmobarFont 2 "\xe0b6")
    
    layoutIcon :: String -> String
    layoutIcon layout = xmobarColor background "" $ case layout of
        "Spacing Tall"        -> "<icon=tiled.xpm/>"
        "Spacing Mirror Tall" -> "<icon=mirrortiled.xpm/>"
        "Spacing Full"        -> "<icon=full.xpm/>"
        _                     -> "Error in myPP"
