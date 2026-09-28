module Theme.Bar
    (
      barFont
    , additionalBarFonts
    , monoFontIdx
    , nerdFontIdx
    , bubbleOffset
    , sepOffset
    , withOffset
    , bubbleBgSpec
    , sepBgSpec
    , barHeight
    , barBg
    , bubbleBg
    , bubbleFg
    , subtleFg
    , dimFg
    , okFg
    , warnFg
    , alertFg
    , borderCol
    , trayMargin
    , trayIconSize
    , trayMaxIcons
    , trayPadProp
    ) where

import           Data.List   (elemIndex, intercalate)
import           Theme.Scale (scaled)
import           Theme.Theme (base01, base02, base03, base07, base08, base0D, base0F,
                              basebg, baseborder)

barFontSize :: Int
barFontSize = 10

xftFont :: String -> String
xftFont family =
    "xft:" <> family <> ":size=" <> show barFontSize <> ":antialias=true:hinting=true"

barFontFamilies :: [String]
barFontFamilies =
    [ "SF Mono"
    , "Twemoji"
    , "Noto Sans Devanagari"
    , "Noto Sans Bengali"
    , "Noto Sans Arabic"
    , "Noto Sans CJK JP"
    , "Noto Sans CJK KR"
    ]

barFont :: String
barFont =
    "xft:" <> intercalate "," barFontFamilies
           <> ":style=Regular:size=" <> show barFontSize
           <> ":antialias=true:hinting=true"

monoFont, nerdFont :: String
monoFont = xftFont "SF Mono"
nerdFont = xftFont "Liga SFMono Nerd Font"

additionalBarFonts :: [String]
additionalBarFonts = [monoFont, nerdFont]

fontIdx :: String -> Int
fontIdx f = maybe 0 (+ 1) (elemIndex f additionalBarFonts)

monoFontIdx, nerdFontIdx :: Int
monoFontIdx = fontIdx monoFont
nerdFontIdx = fontIdx nerdFont

bubbleOffset, sepOffset :: Int
bubbleOffset = scaled 6
sepOffset    = scaled 3.5

barHeight :: Int
barHeight = scaled 30

barBg, bubbleBg, bubbleFg, subtleFg, dimFg, okFg, warnFg, alertFg, borderCol :: String
barBg     = basebg
bubbleBg  = base08
bubbleFg  = base07   -- ordinary widget text
subtleFg  = base0D   -- de-emphasised: the muted speaker, the "-" between fields
dimFg     = base0F   -- one step brighter than 'subtleFg': icons beside live text
okFg      = base02   -- playing, download rate
warnFg    = base03   -- upload rate
alertFg   = base01   -- paused
borderCol = baseborder

withOffset :: String -> Int -> String
withOffset colour offset = colour <> ":" <> show offset

bubbleBgSpec, sepBgSpec :: String
bubbleBgSpec = bubbleBg `withOffset` bubbleOffset
sepBgSpec    = barBg `withOffset` sepOffset

trayMargin :: Int
trayMargin = scaled 6

trayIconSize :: Int
trayIconSize = barHeight - 2 * trayMargin

trayMaxIcons :: Int
trayMaxIcons = 8

trayPadProp :: String
trayPadProp = "_XMONAD_TRAYPAD"