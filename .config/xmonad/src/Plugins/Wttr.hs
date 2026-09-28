module Plugins.Wttr ( Wttr (..) ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             (forever)
import           Data.Maybe                (fromMaybe)
import           Data.Time.LocalTime       (TimeOfDay (todHour), ZonedTime (zonedTimeToLocalTime),
                                            LocalTime (localTimeOfDay), getZonedTime)
import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))
import           System.IO.Error           (catchIOError)
import           System.Process            (readProcessWithExitCode)
import           Theme.Bar                 (bubbleBgSpec, bubbleFg, dimFg)
import           XMonad.Hooks.StatusBar.PP (xmobarColor)
import           Xmobar                    (Exec (..), tenthSeconds)

data Wttr = Wttr deriving (Read, Show)

instance Exec Wttr where
    alias _    = "weather"
    start _ cb = forever $ do
        report <- fetch attempts
        case report of
            Just (code, temperature) -> do
                night <- isNight
                cb (render (iconFor night code) temperature)
                tenthSeconds refreshOk
            Nothing -> cb offline >> tenthSeconds refreshFail

refreshOk, refreshFail, retryGap :: Int
refreshOk   = 15 * minutes
refreshFail = 5  * minutes
retryGap    = 2  * seconds

minutes, seconds :: Int
minutes = 60 * seconds
seconds = 10

attempts :: Int
attempts = 5

offline :: String
offline = "Offline"

fetch :: Int -> IO (Maybe (String, String))
fetch n
    | n <= 0    = pure Nothing
    | otherwise = do
        out <- curl
        case words out of
            [code, temperature] -> pure (Just (code, temperature))
            _                   -> tenthSeconds retryGap >> fetch (n - 1)

curl :: IO String
curl = do
    (code, out, _) <- query `catchIOError` const (pure (ExitFailure 1, "", ""))
    pure $ if code == ExitSuccess then out else ""
  where
    query  = readProcessWithExitCode "curl" args ""
    args = ["-s", "--max-time", "10", "wttr.in/?m&format=%x+%t"]

isNight :: IO Bool
isNight = do
    hour <- todHour . localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime
    pure (hour >= 19 || hour <= 4)

iconFor :: Bool -> String -> String
iconFor night code = fromMaybe code (nightGlyph <|> lookup code dayIcons)
  where
    nightGlyph
        | night     = lookup code nightIcons
        | otherwise = Nothing

render :: String -> String -> String
render glyph temperature =
       xmobarColor dimFg    bubbleBgSpec (glyph <> " ")
    <> xmobarColor bubbleFg bubbleBgSpec (" " <> temperature)

dayIcons :: [(String, String)]
dayIcons =
    [ ("?"  , "\xe370"), ("mm" , "\xe33d"), ("="  , "\xe303")
    , ("///", "\xe318"), ("//" , "\xe317"), ("**" , "\xe31a")
    , ("*/*", "\xe35e"), ("/"  , "\xe308"), ("."  , "\xe307")
    , ("x"  , "\xe306"), ("x/" , "\xe306"), ("*"  , "\xe30a")
    , ("*/" , "\xe35f"), ("m"  , "\xe302"), ("o"  , "\xe30d")
    , ("/!/", "\xe31d"), ("!/" , "\xe31d"), ("*!*", "\xe365")
    , ("mmm", "\xe312")
    ]

nightIcons :: [(String, String)]
nightIcons =
    [ ("="  , "\xe346"), ("/"  , "\xe325"), ("."  , "\xe324")
    , ("x"  , "\xe326"), ("x/" , "\xe326"), ("*"  , "\xe327")
    , ("*/" , "\xe361"), ("m"  , "\xe37e"), ("o"  , "\xe32b")
    , ("*!*", "\xe367")
    ]