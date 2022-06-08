Config { font = "xft:SF Mono Powerline:pixelsize=13:antialias=true:hinting=true"
       , additionalFonts = 
       [ "xft:SF Mono Powerline:pixelsize=13:antialias=true:hinting=true"
         , "xft:SF Mono Powerline:pixelsize=13:antialias=true:hinting=true"
	 , "xft:SF Mono Powerline:pixelsize=15:antialias=true:hinting=true"
	 , "xft:SF Mono Powerline:pixelsize=13:antialias=true:hinting=true"
	 , "xft:SF Mono Powerline:pixelsize=13:antialias=true:hinting=true"
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
       , iconRoot = ".xmonad/xpm/" -- default: "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date "%a, %b %_d, %-l:%M %p" "date" 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run UnsafeXMonadLog
		    , Run Com "/home/sam/.xmonad/xmobar/volume.sh" [] "volume" 10
		    , Run Com "/home/sam/.xmonad/xmobar/gputemp.sh" [] "gpu" 10
		    , Run Com "/home/sam/.xmonad/xmobar/cputemp.sh" [] "cpu" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "\
       \ \
       \<icon=haskell_20.xpm/> \
       \| <fn=5>%UnsafeXMonadLog%</fn>\
       \}
       %date%
       { <fc=#DFDFDF>%cpu%</fc> \
       \| <fc=#DFDFDF>%gpu%</fc> \
       \| <fc=#ffffff>%memory%</fc>  %volume%  " 
       }

