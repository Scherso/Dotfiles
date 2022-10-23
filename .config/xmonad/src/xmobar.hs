import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)

import Xmobar

main :: IO ()
main = xmobar =<< myConfig

myHomeDir :: String
myHomeDir = unsafeDupablePerformIO (getEnv "HOME") 

myConfig :: IO Config
myConfig =
    do
        pure baseConfig
            { template = concat $ 
                [ " <fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn>\
                  \<fn=4><fc=#C678DD,#31353F:5>\xf30d </fc></fn>\
                  \<fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn> "
                ] 
                <>
                [ "<fn=5>%UnsafeXMonadLog%</fn>}{" ] 
                <>
                [ "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn>\
                  \<fn=4><fc=#E06C75,#31353F:5>%wlp5s0%</fc></fn>\
                  \<fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn> "
                ] 
                <>
                [ "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn>\
                   \<fn=4><fc=#56B6C2,#31353F:5>%k10temp%</fc></fn>\
                   \<fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn> "
                ] 
                <>
                [ "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn>\
                   \<fn=4><fc=#C678DD,#31353F:5>%gpu%</fc></fn>\
                   \<fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn> "
                ] 
                <>
                [ "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn>\
                   \<fn=4><fc=#98C379,#31353F:5>%vol%</fc></fn>\
                   \<fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn> "
                ] 
                <>
                [ "<fn=2><fc=#31353F,#282C34:7>\xe0b6</fc></fn>\
                   \<fn=4><fc=#61AFEF,#31353F:5>%date%</fc></fn>\
                   \<fn=2><fc=#31353F,#282C34:7>\xe0b4</fc></fn> "
                ]
            , commands = myCommands
            } 

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