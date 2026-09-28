{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Layouts
    ( myLayoutHook
    , layoutNames
    ) where

import           Theme.Scale                 (scaled)
import           XMonad
import           XMonad.Hooks.ManageDocks    (avoidStruts)
import           XMonad.Hooks.RefocusLast    (refocusLastLayoutHook)
import           XMonad.Layout.Accordion     (Accordion (..))
import           XMonad.Layout.Grid          (Grid (..))
import           XMonad.Layout.NoBorders     (Ambiguity (OnlyScreenFloat), lessBorders)
import           XMonad.Layout.Renamed       (Rename (CutWordsLeft, Replace), renamed)
import           XMonad.Layout.ResizableTile (ResizableTall (..))
import           XMonad.Layout.Spacing       (Border (..), spacingRaw)
import           XMonad.Layout.Spiral        (spiral)
import           XMonad.Layout.ThreeColumns  (ThreeCol (ThreeColMid))

layoutMasterCount :: Int
layoutMasterCount = 1

layoutMasterRatio, layoutDelta :: Rational
layoutMasterRatio = 1 / 2
layoutDelta       = 3 / 100

spiralRatio :: Rational
spiralRatio = 618 / 1000

layoutGap :: Integer
layoutGap = scaled 7

layoutNames :: [String]
layoutNames = ["tall", "mirror", "full", "threecol", "grid", "spiral", "accordion"]

myLayoutHook =
    avoidStruts
    $ lessBorders OnlyScreenFloat
    $ refocusLastLayoutHook
    $ renamed [CutWordsLeft 1]
    $ spacingRaw False (Border layoutGap layoutGap layoutGap layoutGap)
                 True  (Border layoutGap layoutGap layoutGap layoutGap)
                 True
    $ named "tall"      tiled
  ||| named "mirror"    (Mirror tiled)
  ||| named "full"      Full
  ||| named "threecol"  threeCol
  ||| named "grid"      Grid
  ||| named "spiral"    (spiral spiralRatio)
  ||| named "accordion" Accordion
  where
    named n = renamed [Replace n]

    tiled    = ResizableTall layoutMasterCount layoutDelta layoutMasterRatio []
                 :: ResizableTall Window
    threeCol = ThreeColMid layoutMasterCount layoutDelta layoutMasterRatio
                 :: ThreeCol Window