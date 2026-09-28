{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main ( main ) where

import           Bar                           (myStatusBar)
import           Bindings                      (myKeys, myMouseBindings)
import           Layouts                       (myLayoutHook)
import           Manage                        (myLateManageHook, myManageHook)
import           Screens                       (barScreen, otherScreen)
import           Settings
import           System.Environment            (getEnv)
import           Theme.Scale                   (setCursorEnv)
import           Topics                        (checkTopics, myTopics, pruneStaleTopics,
                                                secondaryTopic, topicHistoryHook)
import           Tray                          (syncTray, trayCmd, trayEventHook)
import           XMonad
import           XMonad.Hooks.EwmhDesktops     (ewmh, ewmhFullscreen, setEwmhActivateHook)
import           XMonad.Hooks.ManageDocks      (docks)
import           XMonad.Hooks.OnPropertyChange (onClassChange)
import           XMonad.Hooks.RefocusLast      (isFloat, refocusLastLogHook, refocusLastWhen)
import           XMonad.Hooks.Rescreen         (addAfterRescreenHook, setRescreenDelay)
import           XMonad.Hooks.StatusBar        (dynamicSBs)
import           XMonad.Hooks.UrgencyHook      (NoUrgencyHook (..), doAskUrgent, withUrgencyHook)
import           XMonad.Prelude
import qualified XMonad.StackSet               as W
import           XMonad.Util.Cursor            (setDefaultCursor)
import           XMonad.Util.EZConfig          (additionalKeysP, checkKeymap)
import qualified XMonad.Util.Hacks             as Hacks
import           XMonad.Util.SpawnOnce         (spawnOnce)

main :: IO ()
main = do
    home <- getEnv "HOME"
    xmonad
        . withUrgencyHook NoUrgencyHook
          -- An application asking to be raised (_NET_ACTIVE_WINDOW) gets the
          -- workspace marked urgent rather than being switched to. Discord,
          -- Signal and Steam all do this on a notification, and the default
          -- doFocus yanks the current workspace out from under you when they
          -- do. ppUrgent already renders the mark on the bar.
        . setEwmhActivateHook doAskUrgent
        . docks
        . ewmhFullscreen
        . ewmh
        . Hacks.javaHack
        . setRescreenDelay rescreenDelay
        . addAfterRescreenHook syncTray
        . dynamicSBs (myStatusBar home)
        $ myConfig home

-- | Microseconds to let xrandr events settle before reacting to them. The
-- outputs on this machine re-enumerate on their own and tend to arrive as a
-- burst of events rather than one.
rescreenDelay :: Int
rescreenDelay = 250 * millisecond

-- | One millisecond, in the microseconds 'setRescreenDelay' expects.
millisecond :: Int
millisecond = 1000

myConfig home = def
    { modMask            = myModMask
    , terminal           = myTerminal
    , mouseBindings      = myMouseBindings
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook home
    , handleEventHook    = myEventHook
    , manageHook         = myManageHook
    , logHook            = myLogHook
    , workspaces         = myTopics
    } `additionalKeysP` myKeys

myLogHook :: X ()
myLogHook = do
    refocusLastLogHook
    topicHistoryHook

myEventHook :: Event -> X All
myEventHook = handleEventHook def
    <> trayEventHook
    <> Hacks.windowedFullscreenFixEventHook
    <> Hacks.fixSteamFlicker
      -- Electron and JVM applications set WM_CLASS after the window is
      -- already mapped, by which time manageHook has been and gone. Replay
      -- the rules that name an application whenever a class appears or
      -- changes, so those rules stop being a coin flip.
    <> onClassChange myLateManageHook
    <> refocusLastWhen isFloat

startupApps :: String -> [String]
startupApps home =
    [ "picom"
    , "dunst -conf " <> home <> "/.config/dunst/dunstrc"
    , "gentoo-pipewire-launcher"
    , home <> "/.fehbg"
    , "xset r rate 300 50"
    , "xss-lock -- betterlockscreen -l blur &"
    , "while :; do " <> snixembedBin home <> "; sleep 5; done"
    , trayCmd
    ]

snixembedBin :: String -> String
snixembedBin home =
    "$(command -v " <> home <> "/.local/bin/snixembed || command -v snixembed)"

myStartupHook :: String -> X ()
myStartupHook home = do
    setCursorEnv
    return () >> checkKeymap (myConfig home) myKeys
    checkTopics
    pruneStaleTopics
    traverse_ spawnOnce (startupApps home)
    syncTray
    viewOn otherScreen
    windows $ W.greedyView secondaryTopic
    viewOn barScreen
    setDefaultCursor xC_left_ptr
  where
    viewOn pick = pick >>= \msid ->
        whenJust msid $ \sid -> screenWorkspace sid >>= flip whenJust (windows . W.view)