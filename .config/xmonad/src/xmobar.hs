import System.Environment        (getEnv)
import System.IO.Unsafe          (unsafeDupablePerformIO)

import Xmobar

myHomeDir = unsafeDupablePerformIO (getEnv "HOME") :: String

myConfig :: Config
myConfig =
  defaultConfig
    { font = "xft:SF Mono Nerd Font:pixelsize=14:antialias=true:hinting=true" 
    , additionalFonts = 
    [ "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
    , "xft:SF Mono Nerd Font:size=21"
    , "xft:SF Mono Nerd Font:size=14"
    , "xft:SF Mono Nerd Font:size=14"
    , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
    ]
    , textOffset = 20
    , bgColor = "#282c34"
    , fgColor = "#aab2bf"
    , borderColor = "#544862"
    , border = FullB 
    , borderWidth = 1
    , position = Static { xpos = 10, ypos = 5, width = 1900, height = 32 }
    , alpha = 255
    , overrideRedirect = False
    , lowerOnStart = True
    , hideOnStart = False
    , allDesktops = True
    , persistent = True
    , commands = myCommands
    , sepChar = "%"
    , alignSep = "}{"
    , template ="<fc=#eeeff2,#C678DD:0> \xf30d </fc><fn=2><fc=#C678DD,#282C34:0>\xe0b0</fc></fn>\
                 \<fn=5>%UnsafeXMonadLog%</fn>}{\
                 \<fn=2><fc=#31353f,#282c34:0>\xe0b2</fc></fn><fc=#e06c75,#31353f:0>%wlp5s0% </fc>\
                 \<fn=2><fc=#393f4a,#31353f:0>\xe0b2</fc></fn><fc=#56b6c2,#393f4a:0>%k10temp% </fc>\
                 \<fn=2><fc=#424855,#393f4a:0>\xe0b2</fc></fn><fc=#c678dd,#424855:0>%gpu% </fc>\
                 \<fn=2><fc=#4a5260,#424855:0>\xe0b2</fc></fn><fc=#98c379,#4a5260:0>%vol% </fc>\
                 \<fn=2><fc=#535b6b,#4a5260:0>\xe0b2</fc></fn><fc=#61AFEF,#535b6b:0>%date% </fc>"
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
  , "\xfb19 <Tdie>°C"
  ] 10
  , Run $ Date "\xf017 %a %b %d, %-l:%M %p " "date" 10
  , Run $ Com (myHomeDir <> "/.config/xmonad/scripts/gputemp.sh") ["gpu"] "gpu" 5
  , Run $ Com (myHomeDir <> "/.config/xmonad/scripts/volume.sh" ) ["vol"] "vol" 1
  ] 

main :: IO ()
main = xmobar myConfig
