import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarFont)
import Xmobar

formatbg, foreground, borderc, background :: String
formatbg   = "#31353F"         {- Lighter Grey -}
foreground = "#ABB2BF"         {- White -}
background = "#282C34"         {- Grey -}
borderc    = "#544862"         {- Dark Purple -}

red, blue, green, magenta, cyan :: String -> String
red        = xmobarColor "#E06C75" (formatbg <> ":5")
blue       = xmobarColor "#61AFEF" (formatbg <> ":5") 
green      = xmobarColor "#98C379" (formatbg <> ":5") 
magenta    = xmobarColor "#C678DD" (formatbg <> ":5")
cyan       = xmobarColor "#56B6C2" (formatbg <> ":5")

myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

main :: IO ()
main = xmobar =<< myConfig

myConfig :: IO Config
myConfig = do 
    pure baseConfig
        { template = 
            (wrap "  " " " (xmobarColor "#C678DD" "" (xmobarFont 4 "\xf30d ")))
            <> (inWrapper (xmobarFont 4 "%UnsafeXMonadLog%"))
            <> "}{"
            <> concatMap inWrapper
                [ red     (xmobarFont 4 "%enp6s0%")     {- Received and sent analytics -}
                , cyan    (xmobarFont 4 "%k10temp%")    {- CPU temperature             -} 
                , magenta (xmobarFont 4 "%gpu%")        {- GPU temperature             -}
                , green   (xmobarFont 4 "%vol%")        {- Volume percentage           -}
                , blue    (xmobarFont 4 "%date%")       {- Time                        -}
                ]
        , commands = myCommands
        }
    where
        inWrapper :: String -> String
        inWrapper = wrap 
            (xmobarColor formatbg (background <> ":7") (xmobarFont 2 "\xe0b6"))
            (xmobarColor formatbg (background <> ":7") (xmobarFont 2 "\xe0b4") <> " ")


myCommands :: [Runnable]
myCommands = 
    [ Run UnsafeXMonadLog
    , Run $ Network "enp6s0" 
    [ "-t"
    , "\xf433 <rx> kb \xf431 <tx> kb"
    ] 10
    , Run $ K10Temp "0000:00:18.3"
    [ "-t"
    , "<Tdie>°C"
    ] 10
    , Run $ Date "\xf017 %-l:%M %p" "date" 10
    , Run $ Com (myHomeDir <> "/.config/xmonad/scripts/gputemp.sh") ["gpu"] "gpu" 5
    , Run $ Com (myHomeDir <> "/.config/xmonad/scripts/volume.sh" ) ["vol"] "vol" 1
    ] 

baseConfig :: Config
baseConfig = defaultConfig
    { font            =   "xft:SF Mono:size=11:antialias=true:hinting=true" 
    , additionalFonts = [ "xft:SF Mono:size=11:antialias=true:hinting=true"
                        , "xft:SF Mono:size=13:antialias=true:hinting=true"
                        , "xft:SF Mono:size=11:antialias=true:hinting=true"
                        , "xft:SF Mono:size=11:antialias=true:hinting=true"
                        , "xft:SF Mono:size=11:antialias=true:hinting=true"
                        ]     --      --    
    , textOffsets      = [20, 22, 22, 21, 22]
--  , textOffsets      = [0, 0, 0, 0, 0]
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
