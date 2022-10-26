import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarFont)
import Xmobar

main :: IO ()
main = xmobar =<< myConfig

myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

myConfig :: IO Config
myConfig =
    do
        pure baseConfig
            { template =
                (wrap " " "" (inWrapper (magenta (xmobarFont 4 "\xf30d "))))
                <> (xmobarFont 5 "%UnsafeXMonadLog%")
                <> "}{"
                <> concatMap
                    inWrapper
                    [ red     (xmobarFont 4 "%wlp5s0%")     -- Recieved and sent analytics
                    , cyan    (xmobarFont 4 "%k10temp%")    -- CPU temperature 
                    , magenta (xmobarFont 4 "%gpu%")        -- GPU temperature
                    , green   (xmobarFont 4 "%vol%")        -- Volume percentage
                    , blue    (xmobarFont 4 "%date%")       -- Time
                    ]
            , commands = myCommands
            } 
            where
                inWrapper :: String -> String
                inWrapper = 
	                wrap 
		                (xmobarColor "#31353F" "#282C34:7" (xmobarFont 2 "\xe0b6"))
		                (xmobarColor "#31353F" "#282C34:7" (xmobarFont 2 "\xe0b4") <> " ")

myCommands :: [Runnable]
myCommands = 
    [ Run UnsafeXMonadLog
    , Run $ Network "wlp5s0" 
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
baseConfig =
    defaultConfig
        { font            =   "xft:SF Mono Nerd Font:pixelsize=11:antialias=true:hinting=true" 
        , additionalFonts = [ "xft:SF Mono Nerd Font:pixelsize=10:antialias=true:hinting=true"
                            , "xft:SF Mono Nerd Font:size=13:antialias=true:hinting=true"
                            , "xft:SF Mono Nerd Font:size=11:antialias=true:hinting=true"
                            , "xft:SF Mono Nerd Font:size=11:antialias=true:hinting=true"
                            , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
                            ]
        , textOffsets      = [20, 22, 21, 21, 20]
        , bgColor          = "#282C34"
        , fgColor          = "#AAB2BF"
        , borderColor      = "#544862"
        , border           = FullB 
        , borderWidth      = 1
        {-
        , position         = Static { xpos = 13, ypos = 1034, width = 1893, height = 32 } Bottom Padded
        , position         = Static { xpos = 0, ypos = 1048, width = 1920, height = 32 } Bottom Flat
        , position         = Static { xpos = 0, ypos = 0, width = 1920, height = 32 } Top Flat
        -}
        , position         = Static { xpos = 13, ypos = 8, width = 1893, height = 32 }
        , alpha            = 255
        , overrideRedirect = False
        , lowerOnStart     = True
        , hideOnStart      = False
        , allDesktops      = True
        , persistent       = True
        , iconRoot         = myHomeDir ++ "/.config/xmonad/icons"
        , iconOffset       = -1
        , sepChar  = "%"
        , alignSep = "}{"
        }

foreground, background :: String
foreground = "#31353F" <> ":5"
background = "#282C34"

red, blue, green, magenta, cyan :: String -> String
red     = xmobarColor "#E06C75" foreground 
blue    = xmobarColor "#61AFEF" foreground 
green   = xmobarColor "#98C379" foreground 
magenta = xmobarColor "#C678DD" foreground 
cyan    = xmobarColor "#56B6C2" foreground 