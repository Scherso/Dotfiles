{-# LANGUAGE MultiWayIf, LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans  #-}

{- Data imports -} 
import qualified Data.Map                            as M
import           Data.Functor
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
import           XMonad.Hooks.SetWMName
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

{- Windows key/Super key -}
myModMask :: KeyMask
myModMask = mod4Mask 

{- Default Terminal -}
myTerminal :: String
myTerminal = "alacritty"

{- Default Browser -}
myBrowser :: String
myBrowser = "librewolf" 

{- Workspaces -}
myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9]

{- Border Width -}
myBorderWidth :: Dimension
myBorderWidth = 2 

{- Formal Unfocused Color -}
myNormColor :: String
myNormColor = "#544862" 

{- Focused Color -}
myFocusColor :: String
myFocusColor = "#61AFEF" 

{- Home Directory -}
myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

{- XMobar proprerty variable declaration -}
xmobar :: StatusBarConfig
xmobar = statusBarProp myXmobar myXmobarPP 

{- My XMobar directory -}
myXmobar :: String
myXmobar = ("xmobar " ++ myHomeDir ++ "/.config/xmonad/src/xmobar.hs")

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
    . withEasySB xmobar def
    $ myConfig

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys = base
    ++ window
    ++ applications
    ++ multimedia
    ++ scratchpad
    where 
        {- Force killing a frozen window. -}
        forceKillWindow :: Window -> X ()
        forceKillWindow w = withDisplay $ \d ->
            io $ void $ killClient d w
        {- Float and resize a window to fill a workspace. -}
        toggleFull :: Window -> X () 
        toggleFull w = windows $ \s -> if
            | M.lookup w (W.floating s) == Just fullscreen -> W.sink w s
            | otherwise -> W.float w fullscreen s 
                where
                    fullscreen = W.RationalRect 0 0 1 1
        {- Ensure Rofi spawns in the focused monitor -}
        rofiArgs :: X String
        rofiArgs = (("-show run -monitor " ++) . show) `fmap` curscreen where
            curscreen = ((+1) . fromIntegral . W.screen . W.current) `fmap` gets windowset :: X Int
        rofiCmd :: String -> X ()
        rofiCmd cmd = rofiArgs >>= \args -> spawn $ "rofi " ++ args ++ " " ++ cmd
        {- Screenshots -}
        screenShotSelection  = "screenshot -s" :: String 
        screenShotFullscreen = "screenshot -f" :: String
        base = 
            [ ("M-g",                    withFocused toggleBorder)
            , ("M-S-c",                  kill)
            , ("M-S-x",                  withFocused forceKillWindow)
            , ("M-<Space>",              sendMessage NextLayout)
            , ("M-n",                    refresh)
            , ("M-S-q",                  myQuitHook)
            , ("M-q",                    spawn "xmonad --recompile ; killall xmobar ; xmonad --restart")
            ]
        window = 
            [ ("M-<Tab>",                windows W.focusDown)
            , ("M-j",                    windows W.focusDown)
            , ("M-k",                    windows W.focusUp)
            , ("M-m",                    windows W.focusMaster)
            , ("M-<Return>",             windows W.swapMaster)
            , ("M-S-j",                  windows W.swapDown)
            , ("M-S-k",                  windows W.swapUp)
            , ("M-h",                    sendMessage Shrink)
            , ("M-l",                    sendMessage Expand)
            , ("M-t",                    withFocused $ windows . W.sink)
            , ("M-S-f",                  withFocused toggleFull)
            ]
	scratchpad = 
	    [ ("M-C-<Return>",           namedScratchpadAction myScratchpads "terminal")
	    , ("M-C-<Backspace>",  namedScratchpadAction myScratchpads "htop")
	    ]
        applications =
            [ ("M-S-<Return>",           spawn myTerminal)
            , ("M-f",                    spawn myBrowser)
            , ("M-s",                    spawn screenShotSelection)
            , ("<Print>",                spawn screenShotFullscreen)
            , ("M-p",                    rofiCmd "")
            ]
        multimedia =
            [ ("<XF86AudioPlay>",        spawn "playerctl play-pause")
            , ("M-<Left>",               spawn "playerctl previous")
            , ("M-<Right>",              spawn "playerctl next")
            , ("<XF86AudioPrev>",        spawn "playerctl previous")
            , ("<XF86AudioNext>",        spawn "playerctl next")
            , ("M-<Down>",               spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
            , ("M-<Up>",                 spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
            , ("<XF86AudioMute>",        spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
            , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%-")
            , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%+")
            , ("<Pause>",                spawn "playerctl play-pause")
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
    --setWMName "XMonad LG3D"

myQuitHook :: X ()
myQuitHook = do
    _ <- traverse spawnOnce
        [ "killall picom"
	, "killall pipewire"
	, "killall wireplumber"
	]
    io exitSuccess

myScratchpads :: [NamedScratchpad]
myScratchpads = 
    [ NS "terminal" spawnTerm findTerm manageTerm
    , NS "htop"     spawnHtop findHtop manageHtop
    ]
    where
        spawnTerm  = myTerminal ++ " --class Scratchpad"
	findTerm   = className =? "Scratchpad"
	manageTerm = customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (3 / 4)
	spawnHtop  = myTerminal ++ " --class HTOP -e htop"
	findHtop   = className =? "HTOP"
	manageHtop = customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (3 / 4)

isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title     =? t
isInstance (NameApp n  _) = appName   =? n

type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data App = ClassApp AppClassName AppCommand
    | TitleApp AppTitle AppCommand
    | NameApp AppName AppCommand
    deriving Show

gimp          = ClassApp "Gimp"                         "gimp"
gimp2         = ClassApp "Gimp-2.99"                    "gimp-2.99"
prismlauncher = ClassApp "PrismLauncher"                "prismlauncher"
about         = TitleApp "About LibreWolf"              "About LibreWolf"
signin        = TitleApp "Sign In"                      "Sign In"
spotify       = ClassApp "Spotify"                      "spotify"
toolkit       = TitleApp "Toolkit"                      "Toolkit"
file          = TitleApp "File Upload"                  "File Upload"
save          = TitleApp "Save"                         "Save"
library       = TitleApp "Library"                      "Library"
message       = ClassApp "Xmessage"                     "Xmessage"
steam         = ClassApp "steam"                        "steam" 
obs           = ClassApp "obs"                          "obs"
wine          = TitleApp "Wine System Tray"             "Wine System Tray"
news          = TitleApp "Steam - News"                 "Steam - News"
discUpdate    = TitleApp "Discord Updater"              "Discord Updater"
discord       = ClassApp "vesktop"                      "vesktop"
recaf1        = TitleApp "Remove methods"               "Remove methods"
recaf2        = TitleApp "Remove fields"                "Remove fields"
recaf3        = TitleApp "Rename class"                 "Rename class"
recaf4        = TitleApp "Select a destination package" "Select a destination package"
recaf5        = TitleApp "Remove annotations"           "Remove annotations"
blueman       = ClassApp "Blueman-services"             "Blueman-services"
blueman2      = ClassApp "Blueman-manager"              "Blueman-manager"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = manageRules
    where
        {- Hides windows without ignoring it, see doHideIgnore in XMonad contrib. -}
        doHide :: ManageHook
        doHide = ask >>= doF . W.delete 
        {- WM_WINDOW_ROLE will be parsed with the role variable. -}
        isRole :: Query String 
        isRole = stringProperty "WM_WINDOW_ROLE" 
        {- To match multiple properties with one operator. -}
        anyOf :: [Query Bool] -> Query Bool
        anyOf = foldl (<||>) (pure False) 
        {- To match multiple classNames with one operator. -}
        match :: [App] -> Query Bool
        match = anyOf . fmap isInstance 
        {- Checking for splash dialogs. -}
        isSplash :: Query Bool 
        isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
        {- Checking for pop-ups. -}
        isPopup :: Query Bool 
	isPopup = (isRole =? "pop-up") <||> (isRole =? "Popup")
        {- Checking for file chooser dialog. -}
        isFileChooserDialog :: Query Bool 
        isFileChooserDialog = isRole =? "GtkFileChooserDialog" 
        {- Checking for system info dialogs. -} 
        isSysInfoDialog :: Query Bool 
        isSysInfoDialog = title =? "System information"
        {- Managing rules for applications. -}
        manageRules = composeOne
            [ transience
            , match [ gimp
                    , gimp2
                    , about
                    , message
                    , obs
                    , file
                    , save
                    , signin
                    , toolkit
                    ] -?> doFloat
            , match [ prismlauncher
                    , library
		    , recaf1
		    , recaf2
		    , recaf3
		    , recaf4
		    , recaf5
		    , steam 
		    , blueman
		    , blueman2
                    ] -?> doCenterFloat
            , match [ wine 
                    , news
                    ] -?> doHide
            , match [ discUpdate 
                    ] -?> hasBorder False
            , match [ discord 
                    , spotify
                    ] -?> doShift (myWorkspaces !! 1) -- Map starts at 0, 1 is 2nd workspace. 
            , anyOf [ isFileChooserDialog
                    , isDialog
                    , isPopup
                    , isSplash
                    , isSysInfoDialog
                    ] -?> doCenterFloat
	    , anyOf [ isPopup
	            ] -?> hasBorder False 
	    ] <> composeAll
            [ manageDocks
            , className ^? "jetbrains-"     <&&> title ^? "Welcome to " --> doCenterFloat
            , className ^? "jetbrains-"     <&&> title ^? "splash"      --> (doFloat <+> hasBorder False)
            , className ^? "jetbrains-"     <&&> title ^? "win"         --> hasBorder False
	    , className ^? "software.coley" <&&> title =? ""            --> doCenterFloat
	    , className ^? "software.coley" <&&> title =? "Config"      --> doCenterFloat
            ] <+> namedScratchpadManageHook myScratchpads

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
        tiled   = Tall nmaster delta ratio
        nmaster = 1       {- Default number of windows in the master pane.          -}
        ratio   = 1 / 2   {- Default proportion of screen occupied by master panes. -}
        delta   = 3 / 100 {- Percent of screen increment by when resizing panes.    -}
        w       = 7       {- Width of pixel size between windows while tiled.       -} 

myXmobarPP :: X PP
myXmobarPP = pure (filterOutWsPP [scratchpadWorkspaceTag] myPP) >>= clickablePP
    where
        myPP = def
            { ppCurrent          = xmobarColor blue background . xmobarFont 4
            , ppVisibleNoWindows = Just (xmobarColor magenta background)
            , ppVisible          = xmobarColor blue background 
            , ppHidden           = xmobarColor white background 
            , ppHiddenNoWindows  = xmobarColor magenta1 background 
            , ppUrgent           = xmobarColor red background . wrap "!" "!"
            , ppTitle            = xmobarColor white background . shorten 80  
            , ppSep              = wrapSep " "
            , ppTitleSanitize    = xmobarStrip
            , ppWsSep            = xmobarColor "" background "   "
            , ppLayout           = xmobarColor background "" 
                                   . (\case 
                                       "Spacing Tall"        -> "<icon=tiled.xpm/>"
                                       "Spacing Mirror Tall" -> "<icon=mirrortiled.xpm/>"
                                       "Spacing Full"        -> "<icon=full.xpm/>"
                                     )
            }
            where
                wrapSep :: String -> String
                wrapSep = wrap 
                    (xmobarColor background (basebg <> ":6") (xmobarFont 2 "\xe0b4"))
                    (xmobarColor background (basebg <> ":6") (xmobarFont 2 "\xe0b6"))

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
    , handleEventHook    = myEventHook
    , workspaces         = myWorkspaces
    } `additionalKeysP` myAdditionalKeys
