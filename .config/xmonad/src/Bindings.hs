{-# LANGUAGE FlexibleContexts #-}

{- |
   Note: the media bindings are called 'mediaKeys', not @multimediaKeys@ --
   "XMonad.Prelude" already exports a list by that name.

   @M-1@ .. @M-9@ are bound here rather than left to xmonad's defaults. The
   default bindings only view a workspace; these go through 'goto', which also
   runs the topic's action when the topic is empty. See "Topics".
-}
module Bindings
    ( myKeys
    , myMouseBindings
    , myQuitHook
    ) where

import qualified Data.Map                       as M
import           Layouts                        (layoutNames)
import           Scratchpads                    (myScratchpads)
import           Settings                       (myBrowser)
import           System.Exit                    (exitSuccess)
import           XMonad
import           XMonad.Actions.CycleWS         (Direction1D (..), WSType (..), moveTo, shiftTo)
import           XMonad.Actions.NoBorders       (toggleBorder)
import           XMonad.Actions.PhysicalScreens (onNextNeighbour, onPrevNeighbour)
import           XMonad.Hooks.ManageDocks       (ToggleStruts (..))
import           XMonad.Hooks.RefocusLast       (toggleFocus)
import           XMonad.Layout.ResizableTile    (MirrorResize (..))
import           XMonad.Prelude
import           Topics                         (goto, myTopics, promptedGoto,
                                                 promptedShift, runTopicAction,
                                                 spawnShell, toggleTopic)
import qualified XMonad.StackSet                as W
import           XMonad.Util.NamedScratchpad    (namedScratchpadAction, scratchpadWorkspaceTag)

-- | Workspace cycle predicate: skip the named scratchpad workspace.
notNSP :: WSType
notNSP = WSIs $ pure $ (/= scratchpadWorkspaceTag) . W.tag

myKeys :: [(String, X ())]
myKeys = concat
    [ [ ("M-g",             withFocused toggleBorder)
      , ("M-S-c",           kill)
      , ("M-S-x",           withFocused $ \w -> withDisplay $ \d -> io $ void $ killClient d w)
      , ("M-<Space>",       sendMessage NextLayout)
      , ("M-b",             sendMessage ToggleStruts)
      , ("M-n",             refresh)
      , ("M-S-q",           myQuitHook)
        -- && so that a failed recompile does not silently restart the old
        -- binary; the status bar is restarted by withSB's cleanup hook, so
        -- killing xmobar by hand here would only desync its PID tracking.
      , ("M-q",             spawn "xmonad --recompile && xmonad --restart")
      ]
    , [ ("M-<Tab>",         windows W.focusDown)
        -- Alt-tab between the two most recently focused windows. Depends on
        -- refocusLastLogHook in the log hook for the history it toggles over.
      , ("M-;",             toggleFocus)
        -- The same idea one level up: back to the previous topic on this
        -- screen. Depends on topicHistoryHook in the log hook.
      , ("M-S-;",           toggleTopic)
      , ("M-j",             windows W.focusDown)
      , ("M-k",             windows W.focusUp)
      , ("M-m",             windows W.focusMaster)
      , ("M-<Return>",      windows W.swapMaster)
      , ("M-S-j",           windows W.swapDown)
      , ("M-S-k",           windows W.swapUp)
      , ("M-h",             sendMessage Shrink)
      , ("M-l",             sendMessage Expand)
      , ("M-S-h",           sendMessage MirrorExpand)
      , ("M-S-l",           sendMessage MirrorShrink)
      , ("M-S-n",           spawn "betterlockscreen -l blur")
      , ("M-t",             withFocused $ windows . W.sink)
      , ("M-S-f",           withFocused toggleFull)
      ]
    , [ ("M-C-<Return>",    namedScratchpadAction myScratchpads "terminal")
      , ("M-C-<Backspace>", namedScratchpadAction myScratchpads "htop")
      , ("M-C-s",           namedScratchpadAction myScratchpads "spotify")
      , ("M-C-f",           namedScratchpadAction myScratchpads "librewolf")
      ]
      -- A terminal already in the current topic's directory, rather than
      -- wherever xmonad happened to be started from.
    , [ ("M-S-<Return>",    spawnShell)
      , ("M-f",             spawn myBrowser)
      , ("M-s",             spawn "screenshot -s")
      , ("<Print>",         spawn "screenshot -f")
      , ("M-p",             rofiCmd "")
      ]
    , [ ("M-d",             spawn "dunstctl close")
      , ("M-S-d",           spawn "dunstctl close-all")
      , ("M-`",             spawn "dunstctl history-pop")
      ]
      -- Screens are addressed by physical position, not by ScreenId; see
      -- "Screens" for why the enumeration order cannot be trusted here.
    , [ ("M-]",             moveTo Next notNSP)
      , ("M-[",             moveTo Prev notNSP)
      , ("M-S-]",           shiftTo Next notNSP >> moveTo Next notNSP)
      , ("M-S-[",           shiftTo Prev notNSP >> moveTo Prev notNSP)
      , ("M-.",             onNextNeighbour def W.view)
      , ("M-,",             onPrevNeighbour def W.view)
      , ("M-S-.",           onNextNeighbour def W.shift >> onNextNeighbour def W.view)
      , ("M-S-,",           onPrevNeighbour def W.shift >> onPrevNeighbour def W.view)
      ]
      -- Seven layouts is too many to reach by cycling with M-<Space>, so each
      -- one also gets a direct binding. Generated from the same list "Bar"
      -- draws icons from, so the two cannot disagree.
    , [ ("M-C-" <> show i, sendMessage (JumpToLayout n))
      | (i, n) <- zip [(1 :: Int) ..] layoutNames
      ]
    , [ ("M-a",             runTopicAction)
      , ("M-o",             promptedGoto)
      , ("M-S-o",           promptedShift)
      ]
      -- Generated from the topic list, so the digits cannot drift out of
      -- step with it. M-S-<digit> is left to xmonad's own bindings, which
      -- already shift to whatever the Nth workspace is.
    , [ ("M-" <> show i, goto topic)
      | (i, topic) <- zip [(1 :: Int) ..] myTopics
      ]
    , mediaKeys
    ]
  where
    -- One binding: the rect written by the float branch is the same value the
    -- test branch compares against, so the toggle cannot drift out of sync.
    fullscreenRect = W.RationalRect 0 0 1 1

    toggleFull w = windows $ \s ->
        if M.lookup w (W.floating s) == Just fullscreenRect
            then W.sink w s
            else W.float w fullscreenRect s

    -- rofi numbers monitors in its own detection order, which is unrelated to
    -- xmonad's ScreenId (and ScreenId is not stable here anyway -- see
    -- "Screens"). On this machine the old ScreenId+1 arithmetic could ask for
    -- monitor 2 when rofi only knows 0 and 1. -1 means "the currently focused
    -- monitor", which is what the arithmetic was reaching for.
    rofiCmd cmd = spawn $ "rofi -show run -monitor -1 " <> cmd

mediaKeys :: [(String, X ())]
mediaKeys =
    [ ("<XF86AudioPlay>",   spawn "playerctl play-pause")
    , ("<Pause>",           spawn "playerctl play-pause")
    , ("M-<Left>",          spawn "playerctl previous")
    , ("M-<Right>",         spawn "playerctl next")
    , ("<XF86AudioPrev>",   spawn "playerctl previous")
    , ("<XF86AudioNext>",   spawn "playerctl next")
    ] <> volumeKeys
  where
    volumeKeys =
        [ (k, spawn $ "wpctl set-" <> cmd <> " @DEFAULT_AUDIO_SINK@ " <> arg)
        | (k, cmd, arg) <- [ ("M-<Down>",               "volume", "5%-")
                           , ("M-<Up>",                 "volume", "5%+")
                           , ("<XF86AudioMute>",        "mute",   "toggle")
                           , ("<XF86AudioLowerVolume>", "volume", "2%-")
                           , ("<XF86AudioRaiseVolume>", "volume", "2%+")
                           ]
        ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w   >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myQuitHook :: X ()
myQuitHook = do
    traverse_ (spawn . ("killall " <>)) ["picom", "pipewire", "wireplumber"]
    io exitSuccess