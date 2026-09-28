module Main ( main ) where

import           Data.List                 (minimumBy)
import           Data.Maybe                (fromMaybe)
import           Data.Ord                  (comparing)
import           Plugins.NowPlaying        (NowPlaying (..))
import           Plugins.SinkVolume        (SinkVolume (..))
import           Plugins.Wttr              (Wttr (..))
import           System.Environment        (getEnv)
import           System.IO.Error           (catchIOError)
import           Text.Read                 (readMaybe)
import           Theme.Bar
import           Theme.Scale               (scaled)
import           Theme.Theme               (basefg)
import           XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarFont)
import           Xmobar

-- | xmobar poll intervals are counted in tenths of a second, which is easy
-- to misread as either milliseconds or seconds.
seconds :: Int -> Int
seconds n = n * 10

main :: IO ()
main = do
    home  <- getEnv "HOME"
    iface <- defaultInterface
    xmobar =<< configFromArgs (myConfig home iface)

myConfig :: String -> Maybe String -> Config
myConfig home iface = baseConfig
    { template = myTemplate iface
    , commands = myCommands iface
    , iconRoot = home <> "/.config/xmonad/icons"
    }

defaultInterface :: IO (Maybe String)
defaultInterface = do
    txt <- readFile "/proc/net/route" `catchIOError` const (pure "")
    let routes = [ (dev, metric)
                 -- Iface Destination Gateway Flags RefCnt Use Metric ...
                 | row <- drop 1 (lines txt)
                 , dev : dest : _gw : _flags : _ref : _use : metricStr : _ <- [words row]
                 , dest == "00000000"
                 , Just metric <- [readMaybe metricStr :: Maybe Int]
                 ]
    pure $ if null routes
               then Nothing
               else Just (fst (minimumBy (comparing snd) routes))

netAlias :: Maybe String -> String
netAlias = fromMaybe "dynnetwork"

netCommand :: Maybe String -> Runnable
netCommand (Just dev) = Run $ Network dev ["-t", netTemplate] (seconds 1)
netCommand Nothing    = Run $ DynNetwork   ["-t", netTemplate] (seconds 1)

inBubble :: String -> String
inBubble = wrap capLeft (capRight <> " ")
  where
    capLeft  = xmobarColor bubbleBg (barBg `withOffset` bubbleOffset) (xmobarFont nerdFontIdx "\xe0b6")
    capRight = xmobarColor bubbleBg (barBg `withOffset` bubbleOffset) (xmobarFont nerdFontIdx "\xe0b4")

onBubble :: String -> String
onBubble = xmobarColor bubbleFg bubbleBgSpec

var :: String -> String
var = wrap "%" "%"

myTemplate :: Maybe String -> String
myTemplate iface =
       wrap "  " " " (xmobarColor subtleFg "" (xmobarFont nerdFontIdx "\xe61f "))
    <> inBubble (var "UnsafeXMonadLog")
    <> wrap "}" "{" (var "date")
    <> concatMap (inBubble . onBubble) monitors
    <> onBubble (var (alias NowPlaying))
    <> var trayPadProp
  where
    monitors = map var [netAlias iface, alias Wttr, alias SinkVolume]

netTemplate :: String
netTemplate =
       netIcon okFg   "\xf433 " <> " <rx> kb "
    <> netIcon warnFg "\xf431 " <> " <tx> kb"
  where
    netIcon colour = xmobarFont nerdFontIdx . xmobarColor colour bubbleBgSpec

-- | The widgets, including the three in "Plugins" that used to be shell
-- scripts behind a 'CommandReader'. Being real 'Exec' instances gets them the
-- shared palette out of "Theme.Bar" and the type checker over their markup,
-- and drops three persistent @bash@ processes off the session.
myCommands :: Maybe String -> [Runnable]
myCommands iface =
    [ Run UnsafeXMonadLog
    , netCommand iface
    , Run $ Date "%H:%M:%S" "date" (seconds 1)
      -- Room for the system tray. xmonad measures the tray and writes
      -- "<hspace=N/>" here whenever it resizes (see the Tray module); this
      -- replaces a shell script that xmobar re-ran twice a second.
    , Run $ XPropertyLog trayPadProp
    , Run SinkVolume
    , Run NowPlaying
    , Run Wttr
    ]

baseConfig :: Config
baseConfig = defaultConfig
    { font             = barFont
    , additionalFonts  = additionalBarFonts
    , textOffsets      = []
    , bgColor          = barBg
    , fgColor          = basefg
    , borderColor      = borderCol
    , border           = BottomB
    , borderWidth      = scaled 1
      -- Fallback only, for running xmobar by hand: xmonad overrides this
      -- with an exact Static rectangle. TopSize treats its height argument as
      -- a minimum and would inflate the bar to the font's height.
    , position         = TopSize L 100 barHeight
    , alpha            = 255
    , overrideRedirect = False
    , lowerOnStart     = True
    , hideOnStart      = False
    , allDesktops      = False
    , persistent       = True
    , iconOffset       = -1
    , sepChar          = "%"
    , alignSep         = "}{"
    }