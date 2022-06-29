Config { font		   = "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
       , additionalFonts = [ "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
        		   , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
	                   , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
	                   , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
                           , "xft:SF Mono Nerd Font:pixelsize=13:antialias=true:hinting=true"
                           ]
       , borderColor = "#545862"
       -- , border = FullBM 0
       -- , borderWidth = 3
       , bgColor = "#282c34"
       , fgColor = "#aab2bf"
       , alpha = 255
       , position = Static { xpos = 10, ypos = 5, width = 1900, height = 30 }
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = ".config/xmonad/xpm/" -- default: "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ 
		      Run Date "  %a %b %d - %-l:%M %p " "date" 10 
		    , Run Network "wlp5s0" ["-t"," <rx> kb   <tx> kb"] 10
		    , Run Com "/home/sam/.config/xmonad/xmobar/gputemp.sh" [] "gpu" 10
		    , Run Com "/home/sam/.config/xmonad/xmobar/cputemp.sh" [] "cpu" 10
	            , Run Com "/home/sam/.config/xmonad/xmobar/volume.sh"  [] "volume" 1 
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "\
       		    \ <fn=5>%UnsafeXMonadLog%</fn>\
       		    \}
       		    <box type=Bottom offset=C9 width=3 color=#79859d><fc=#79859d>%date%</fc></box>
      		    { 
       		    <box type=Bottom offset=C9 width=3 color=#97a6c4><fc=#97a6c4>%wlp5s0%</fc></box> \
       		    \  <box type=Bottom offset=C9 width=3 color=#98C379><fc=#98C379>%cpu%</fc></box> \
       	            \  <box type=Bottom offset=C9 width=3 color=#61AFEF><fc=#61AFEF>%gpu%</fc></box> \
                    \  <box type=Bottom offset=C9 width=3 color=#C678DD><fc=#C678DD>%volume%</fc></box>  "  
       }
