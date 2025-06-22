import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)
import Theme.Theme

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarFont)
import Xmobar

background, foreground, borderc :: String
background = basebg
foreground = basefg 
borderc    = "#544862" -- Dark Purple

white :: String -> String
white      = xmobarColor base07 (base08 <> ":5") 

myHomeDir :: String
myHomeDir  = unsafeDupablePerformIO (getEnv "HOME")

fonts :: [String]
fonts = map (++ ":size=11:antialias=true:hinting=true") 
    [ "SF Mono" 
    , "Twemoji"
    , "Noto Sans Devanagari"
    , "Noto Sans Bengali"
    , "Noto Sans Arabic"
    , "Noto Sans CJK JP"
    , "Noto Sans CJK KR"
    ]

additional_fonts :: [String]
additional_fonts = map (\size -> "xft:SF Mono:size=" ++ show size ++ ":antialias=true:hinting=true") [11, 13 :: Int]

main :: IO ()
main = xmobar =<< myConfig

myConfig :: IO Config
myConfig = pure baseConfig
    { template = makeTemplate
    , commands = myCommands
    }

makeTemplate :: String
makeTemplate = 
    wrap "  " " " (xmobarColor "#6B7089" "" (xmobarFont 2 "\xe61f "))
    <> inWrapper (xmobarFont 4 "%UnsafeXMonadLog%")
    <> wrap "}" "{" (xmobarFont 4 "%date%")
    <> concatMap (inWrapper . white . xmobarFont 4) monitors
    <> white (xmobarFont 4 "%playerctl%")
  where
    monitors = ["%enp7s0%", "%weather%", "%volume%"]
    inWrapper = wrap 
        (xmobarColor base08 (background <> ":7") (xmobarFont 2 "\xe0b6"))
        (xmobarColor base08 (background <> ":7") (xmobarFont 2 "\xe0b4") <> " ")

myCommands :: [Runnable]
myCommands = 
    [ Run UnsafeXMonadLog
    , Run $ Network "enp7s0" netOpts 10
    , Run $ Date "%H:%M:%S" "date" 10
    ] ++ map mkCmdReader ["volume", "playerctl", "weather"]
  where
    netOpts = ["-t", "<fn=2><fc=#98C379,#31353F:5>\xf433</fc></fn> <rx> kb <fn=2><fc=#E5C07B,#31353F:5>\xf431</fc></fn> <tx> kb"]
    mkCmdReader script_name = Run $ CommandReader 
        (script script_name) script_name
    script script_name = "exec " <> myHomeDir <> "/.config/xmonad/scripts/" <> script_name <> ".sh"

baseConfig :: Config
baseConfig = defaultConfig
    { font             = concatMap (\f -> "xft:" ++ f ++ ",") fonts 
    , additionalFonts  = additional_fonts
    , textOffsets      = [20, 22, 22, 21, 22]
    , bgColor          = background 
    , fgColor          = foreground 
    , borderColor      = borderc
--  , border           = FullB
    , border           = BottomB 
    , borderWidth      = 1
--  , position         = Static { xpos = 1933, ypos = 8, width = 2533, height = 32 }
    , position         = Static { xpos = 1920, ypos = 0, width = 2560, height = 32 }
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
