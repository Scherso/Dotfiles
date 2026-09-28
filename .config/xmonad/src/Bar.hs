module Bar ( myStatusBar ) where

import           Layouts                          (layoutNames)
import           Screens                          (barScreen, screenRectOf)
import           Theme.Bar
import           Theme.Theme                      (base01, base04, base05, base07, base0D)
import           XMonad
import           XMonad.Hooks.StatusBar           (StatusBarConfig, statusBarProp)
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Util.ClickableWorkspaces  (clickablePP)
import           XMonad.Util.NamedScratchpad      (scratchpadWorkspaceTag)

red, blue, magenta, magenta1, white :: String
red      = base01
blue     = base04
magenta  = base05
magenta1 = base0D
white    = base07

titleMaxLen :: Int
titleMaxLen = 55

myStatusBar :: String -> ScreenId -> X StatusBarConfig
myStatusBar home sid = do
    target <- barScreen
    mrect  <- screenRectOf sid
    pure $ case (target == Just sid, mrect) of
        (True, Just r) -> statusBarProp (xmobarCmd home r) myXmobarPP
        _              -> mempty

xmobarCmd :: String -> Rectangle -> String
xmobarCmd home r = unwords
    [ "xmobar"
    , "-p", "'" <> staticPos <> "'"
    , home <> "/.config/xmonad/src/xmobar.hs"
    ]
  where
    staticPos = concat
        [ "Static { xpos = ", show (rect_x r)
        , ", ypos = ",        show (rect_y r)
        , ", width = ",       show (rect_width r)
        , ", height = ",      show barHeight
        , " }"
        ]

myXmobarPP :: X PP
myXmobarPP = clickablePP $ filterOutWsPP [scratchpadWorkspaceTag] myPP

myPP :: PP
myPP = def
    { ppCurrent          = colorize red
    , ppVisibleNoWindows = Just $ colorize magenta
    , ppVisible          = colorize blue
    , ppHidden           = colorize white
    , ppHiddenNoWindows  = colorize magenta1
    , ppUrgent           = colorFont red monoFontIdx . wrap "!" "!"
    , ppTitle            = colorize white . shorten titleMaxLen
    , ppSep              = wrapSep
    , ppTitleSanitize    = xmobarStrip
    , ppWsSep            = xmobarColor "" bubbleBgSpec "   "
    , ppLayout           = layoutIcon
    }
  where
    colorize :: String -> String -> String
    colorize colour = xmobarColor colour bubbleBgSpec

    colorFont :: String -> Int -> String -> String
    colorFont colour font = xmobarColor colour bubbleBgSpec . xmobarFont font

    wrapSep :: String
    wrapSep = wrap sepLeft sepRight " "
      where
        sepLeft  = xmobarColor bubbleBg sepBgSpec (xmobarFont nerdFontIdx "\xe0b4")
        sepRight = xmobarColor bubbleBg sepBgSpec (xmobarFont nerdFontIdx "\xe0b6")

    -- The icon file is named after the layout, so there is no table here to
    -- fall out of step with "Layouts". Anything unrecognised falls back to
    -- its own name rather than to a missing icon.
    layoutIcon :: String -> String
    layoutIcon layout
        | layout `elem` layoutNames = "<icon=" <> layout <> ".xpm/>"
        | otherwise                 = layout