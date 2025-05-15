import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)
import Theme.Theme

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarFont)
import Xmobar

background, foreground, borderc :: String
background = basebg
foreground = basefg 
borderc    = "#544862" {- Dark Purple  -}

white :: String -> String
white      = xmobarColor base07 (base08 <> ":5") 

myHomeDir :: String
myHomeDir  = unsafeDupablePerformIO (getEnv "HOME") 

main :: IO ()
main = xmobar =<< myConfig

myConfig :: IO Config
myConfig = do 
    pure baseConfig
        { template = 
            (wrap "  " " " (xmobarColor "#6B7089"  "" (xmobarFont 5 "\xe61f ")))
            <> (inWrapper (xmobarFont 4 "%UnsafeXMonadLog%"))
            <> wrap "}" "{" (xmobarFont 4 "%date%")
            <> concatMap inWrapper
                [ white (xmobarFont 4 "%enp7s0%")     {- Received and sent analytics -}
                , white (xmobarFont 4 "%wttr%")       {- Weather information         -}
                , white (xmobarFont 4 "%vol%")        {- Volume percentage           -}
                ]
            <> white (xmobarFont 4 "%playerctl%")     {- Spotify information         -}
        , commands = myCommands
        }
    where
        inWrapper :: String -> String
        inWrapper = wrap 
            (xmobarColor base08 (background <> ":7") (xmobarFont 2 "\xe0b6"))
            (xmobarColor base08 (background <> ":7") (xmobarFont 2 "\xe0b4") <> " ")


myCommands :: [Runnable]
myCommands = 
    [ Run UnsafeXMonadLog
    , Run $ Network "enp7s0" 
    [ "-t"
    , "<fn=2><fc=#98C379,#31353F:5>\xf433</fc></fn> <rx> kb <fn=2><fc=#E5C07B,#31353F:5>\xf431</fc></fn> <tx> kb"
    ] 10
    , Run $ Date "%H:%M:%S" "date" 10
    , Run $ CommandReader ("exec " <> myHomeDir <> "/.config/xmonad/scripts/volume.sh") "vol" 
    , Run $ CommandReader ("exec " <> myHomeDir <> "/.config/xmonad/scripts/playerctl.sh") "playerctl"  
    , Run $ CommandReader ("exec " <> myHomeDir <> "/.config/xmonad/scripts/weather.sh") "wttr"
    ] 

baseConfig :: Config
baseConfig = defaultConfig
    { font            = concatMap fontWrap 
                        [ "xft:SF Mono:size=11:antialias=true:hinting=true"
			, "Twemoji:size=11"
			, "Noto Sans Bengali:size=10:style=Bold"
                        , "Noto Sans Arabic:size=10:style=Bold"
                        , "Noto Color Emoji:size=10:style=Regular"
                        , "Noto Sans CJK JP:size=10:style=Bold"
                        , "Noto Sans CJK KR:size=10:style=Bold"
                        ]
    , additionalFonts = [ "xft:SF Mono:size=11:antialias=true:hinting=true"
                        , "xft:SF Mono:size=13:antialias=true:hinting=true"
                        ]     --      --
    , textOffsets      = [20, 22, 22, 21, 22]
    , bgColor          = background 
    , fgColor          = foreground 
    , borderColor      = borderc
    , border           = FullB 
    , borderWidth      = 1
    {-
    , position         = Static { xpos = 13, ypos = 1034, width = 1893, height = 32 } Bottom Padded
    , position         = Static { xpos = 0, ypos = 1048, width = 1920, height = 32 } Bottom Flat
    , position         = Static { xpos = 0, ypos = 0, width = 1920, height = 32 } Top Flat
    -}
    , position         = Static { xpos = 1933, ypos = 8, width = 2533, height = 32 }
--  , position         = Static { xpos = 0, ypos = 0, width = 2560, height = 32 }
    , alpha            = 255
    , overrideRedirect = False
    , lowerOnStart     = True
    , hideOnStart      = False
    , allDesktops      = False
    , persistent       = True
    , iconRoot         = myHomeDir ++ "/.config/xmonad/icons"
    , iconOffset       = -1
    , sepChar  = "%"
    , alignSep = "}{"
    }
    where
        fontWrap :: String -> String
        fontWrap = wrap "" ","

