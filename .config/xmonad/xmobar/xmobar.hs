Config { font = "xft:SF Mono Nerd Font:pixelsize=14:antialias=true:hinting=true" 
       , additionalFonts = 
       [ "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
       , "xft:SF Mono Nerd Font:size=21"
       , "xft:SF Mono Nerd Font:size=14"
       , "xft:SF Mono Nerd Font:size=14"
       , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
       ]
       , bgColor = "#282c34"
       , fgColor = "#aab2bf"
     --  , position = Static { xpos = 10, ypos = 5, width = 1900, height = 32 }
       , position = Static { xpos = 0, ypos = 0, width = 1920, height = 32 } 
       , overrideRedirect = False
       , lowerOnStart = False
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = 
       [ Run Date "  %a %b %d - %-l:%M %p " "date" 50 
       , Run Network "wlp5s0" ["-t"," <rx> kb   <tx> kb"] 10
       , Run Com "/home/sam/.config/xmonad/xmobar/gputemp.sh" [] "gpu" 10
       , Run K10Temp "0000:00:18.3" ["--template", "﨎 <Tdie>ºC"] 10
       , Run Com "/home/sam/.config/xmonad/xmobar/volume.sh"  [] "volume" 1
       , Run UnsafeXMonadLog
       ]
       , sepChar = "%"
       , alignSep = "}{"
       , template ="<fc=#eeeff2,#C678DD:0>  </fc><fn=2><fc=#C678DD,#282C34:0></fc></fn>\
                   \<fn=5>%UnsafeXMonadLog%</fn>}{\
                   \<fn=2><fc=#31353f,#282c34:0></fc></fn><fc=#e06c75,#31353f:0>%wlp5s0% </fc>\
                   \<fn=2><fc=#393f4a,#31353f:0></fc></fn><fc=#56b6c2,#393f4a:0>%k10temp% </fc>\
                   \<fn=2><fc=#424855,#393f4a:0></fc></fn><fc=#c678dd,#424855:0>  %gpu% </fc>\
                   \<fn=2><fc=#4a5260,#424855:0></fc></fn><fc=#98c379,#4a5260:0>%volume% </fc>\
                   \<fn=2><fc=#535b6b,#4a5260:0></fc></fn><fc=#61AFEF,#535b6b:0>%date% </fc>"
       } -- 
