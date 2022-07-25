Config { font              = "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
       , additionalFonts = [ "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
                           , "xft:SF Mono Nerd Font:size=21"
                           , "xft:SF Mono Nerd Font:size=13"
                           , "xft:SF Mono Nerd Font:size=13"
                           , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
                           ]
       , bgColor = "#282c34"
       , fgColor = "#aab2bf"
       , position = Static { xpos = 10, ypos = 5, width = 1900, height = 30 }
       , overrideRedirect = False
       , lowerOnStart = False
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [
                      Run Date "  %a %b %d - %-l:%M %p " "date" 50 
                    , Run Network "wlp5s0" ["-t"," <rx> kb   <tx> kb"] 10
                    , Run Com "/home/sam/.config/xmonad/xmobar/gputemp.sh" [] "gpu" 10
                    , Run K10Temp "0000:00:18.3" ["--template", "﨎 <Tdie>ºC"] 10
                    , Run Com "/home/sam/.config/xmonad/xmobar/volume.sh"  [] "volume" 1
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template ="<fn=5>%UnsafeXMonadLog%</fn>}{\
                   \<fn=2><fc=#333842,#282c34:0></fc></fn><fc=#e06c75,#333842:0>%wlp5s0% </fc>\
                   \<fn=2><fc=#3e4451,#333842:0></fc></fn><fc=#56b6c2,#3e4451:0>%k10temp% </fc>\
                   \<fn=2><fc=#49515f,#3e4451:0></fc></fn><fc=#c678dd,#49515f:0>  %gpu% </fc>\
                   \<fn=2><fc=#545d6d,#49515f:0></fc></fn><fc=#98c379,#545d6d:0>%date%  <fc=#61AFEF,#545d6d:0>%volume%  </fc></fc>"
       }

