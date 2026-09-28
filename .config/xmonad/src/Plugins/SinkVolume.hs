module Plugins.SinkVolume ( SinkVolume (..) ) where

import           Control.Exception         (bracket)
import           Control.Monad             (forever, void)
import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))
import           System.IO                 (BufferMode (LineBuffering), Handle, hClose,
                                            hGetLine, hSetBuffering)
import           System.IO.Error           (catchIOError)
import           System.Process            (CreateProcess (std_out), ProcessHandle,
                                            StdStream (CreatePipe), createProcess, proc,
                                            readProcessWithExitCode, terminateProcess,
                                            waitForProcess)
import           Text.Read                 (readMaybe)
import           Theme.Bar                 (bubbleBgSpec, bubbleFg, dimFg, nerdFontIdx,
                                            subtleFg)
import           XMonad.Hooks.StatusBar.PP (xmobarColor, xmobarFont)
import           Xmobar                    (Exec (..), tenthSeconds)

data SinkVolume = SinkVolume deriving (Read, Show)

instance Exec SinkVolume where
    alias _    = "volume"
    start _ cb = forever $ do
        emit
        watch `catchIOError` const (pure ())
        tenthSeconds monitorRetry
      where
        emit  = readVolume >>= cb . render
        watch = withMonitor $ \h -> forever (hGetLine h >> emit)

monitorRetry :: Int
monitorRetry = 50

sinkName :: String
sinkName = "@DEFAULT_AUDIO_SINK@"

readVolume :: IO (Maybe (Int, Bool))
readVolume = do
    (code, out, _) <- query `catchIOError` const (pure (ExitFailure 1, "", ""))
    pure $ if code == ExitSuccess then parseVolume out else Nothing
  where
    query = readProcessWithExitCode "wpctl" ["get-volume", sinkName] ""

parseVolume :: String -> Maybe (Int, Bool)
parseVolume out = case words out of
    "Volume:" : level : rest -> toPercent rest <$> readMaybe level
    _                        -> Nothing
  where
    toPercent rest level = (round (level * 100 :: Double), "[MUTED]" `elem` rest)

render :: Maybe (Int, Bool) -> String
render Nothing               = ""
render (Just (_, True))      = icon subtleFg muteGlyph
render (Just (percent, False))
    | percent > 0 = icon dimFg    speaker <> label bubbleFg percent
    | otherwise   = icon subtleFg speaker <> label subtleFg percent
  where
    speaker = speakerGlyph <> " "

icon :: String -> String -> String
icon colour glyph = xmobarColor colour bubbleBgSpec (xmobarFont nerdFontIdx glyph)

label :: String -> Int -> String
label colour percent = xmobarColor colour bubbleBgSpec (show percent <> "%")

muteGlyph, speakerGlyph :: String
muteGlyph    = "\xf026"
speakerGlyph = "\xf028"

monitorProcess :: CreateProcess
monitorProcess =
    (proc "stdbuf" ["-oL", "alsactl", "monitor", "default"]) { std_out = CreatePipe }

withMonitor :: (Handle -> IO a) -> IO a
withMonitor k = bracket (createProcess monitorProcess) stop $ \(_, out, _, _) ->
    case out of
        Nothing -> ioError (userError "alsactl monitor: no stdout")
        Just h  -> hSetBuffering h LineBuffering >> k h
  where
    stop :: (a, Maybe Handle, b, ProcessHandle) -> IO ()
    stop (_, out, _, ph) = do
        terminateProcess ph
        maybe (pure ()) hClose out
        void (waitForProcess ph)