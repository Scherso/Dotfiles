module Scratchpads ( myScratchpads ) where

import           Settings                    (myTerminal)
import qualified XMonad.StackSet             as W
import           XMonad                      (className, (=?))
import           XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating)

scratchpadRect :: W.RationalRect
scratchpadRect = centered (2 / 3) (3 / 4)
  where
    centered w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

librewolfScratchClass :: String
librewolfScratchClass = "LibreWolfScratch"

librewolfScratchCmd :: String
librewolfScratchCmd = unwords
    [ "mkdir -p", librewolfScratchProfile, "&&"
    , "librewolf"
    , "--class",   librewolfScratchClass
    , "--no-remote"
    , "--profile", librewolfScratchProfile
    ]

librewolfScratchProfile :: String
librewolfScratchProfile = "\"$HOME/.librewolf/scratchpad\""

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "terminal"  (myTerminal <> " --class Scratchpad")   (className =? "Scratchpad")           floatConf
    , NS "htop"      (myTerminal <> " --class HTOP -e htop") (className =? "HTOP")                 floatConf
    , NS "librewolf" librewolfScratchCmd                     (className =? librewolfScratchClass)  floatConf
    , NS "spotify"   "spotify"                               (className =? "spotify")              floatConf
    ]
  where
    floatConf = customFloating scratchpadRect