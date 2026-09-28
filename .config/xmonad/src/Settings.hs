module Settings
    ( myModMask
    , myTerminal
    , myBrowser
    , myOffice
    , myBorderWidth
    , myNormColor
    , myFocusColor
    ) where

import           Theme.Scale (scaled)
import           Theme.Theme (base04, baseborder)
import           XMonad      (KeyMask, Dimension, mod4Mask)

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal, myBrowser, myOffice :: String
myTerminal = "alacritty"
myBrowser  = "librewolf"
myOffice   = "libreoffice"

myBorderWidth :: Dimension
myBorderWidth = scaled 2

myNormColor, myFocusColor :: String
myNormColor  = baseborder
myFocusColor = base04