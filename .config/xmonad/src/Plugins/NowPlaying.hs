module Plugins.NowPlaying ( NowPlaying (..) ) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (SomeAsyncException, bracket, evaluate,
                                            fromException, throwIO, try)
import           Control.Monad             (forever, void)
import           Data.IORef                (IORef, atomicWriteIORef, newIORef, readIORef)
import           Data.List                 (isPrefixOf)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           System.IO                 (BufferMode (LineBuffering), Handle, hClose,
                                            hGetLine, hSetBuffering)
import           System.Process            (CreateProcess (std_out), ProcessHandle,
                                            StdStream (CreatePipe), createProcess, proc,
                                            terminateProcess, waitForProcess)
import           Theme.Bar                 (alertFg, barBg, bubbleBgSpec, bubbleFg, dimFg,
                                            nerdFontIdx, okFg, sepBgSpec, subtleFg)
import           XMonad.Hooks.StatusBar.PP (xmobarColor, xmobarFont)
import           Xmobar                    (Exec (..), tenthSeconds)

data Track = Track
    { trackStatus :: !String
    , trackTitle  :: !String
    , trackArtist :: !String
    }

data NowPlaying = NowPlaying deriving (Read, Show)

instance Exec NowPlaying where
    alias _    = "playerctl"
    start _ cb = do
        current <- newIORef Nothing
        void (forkIO (follow current))
        forever $ do
            track <- readIORef current
            now   <- floor <$> getPOSIXTime
            cb (maybe "" (render now) track)
            tenthSeconds (interval track)

player :: String
player = "spotify"

scrollSpeed, idleSpeed, followRetry :: Int
scrollSpeed = 5
idleSpeed   = 20
followRetry = 50

scrollLength :: Int
scrollLength = 25

interval :: Maybe Track -> Int
interval (Just t) | length (trackTitle t) > scrollLength = scrollSpeed
interval _                                               = idleSpeed

fieldSep :: Char
fieldSep = '\x1f'

followProcess :: CreateProcess
followProcess = (proc "stdbuf" args) { std_out = CreatePipe }
  where
    args   = [ "-oL", "playerctl", "--player=" <> player
             , "--follow", "--format", format, "metadata" ]
    format = "{{status}}" <> [fieldSep] <> "{{title}}" <> [fieldSep] <> "{{artist}}"

follow :: IORef (Maybe Track) -> IO ()
follow current = forever $ do
    ignoreSync read'
    atomicWriteIORef current Nothing
    tenthSeconds followRetry
  where
    read' = withFollower $ \h ->
        forever $ do
            line  <- hGetLine h
            track <- evaluate (parseTrack line)
            atomicWriteIORef current track

ignoreSync :: IO () -> IO ()
ignoreSync act = try act >>= either rethrowAsync pure
  where
    rethrowAsync e
        | Just async' <- fromException e :: Maybe SomeAsyncException = throwIO async'
        | otherwise                                                  = pure ()

withFollower :: (Handle -> IO a) -> IO a
withFollower k = bracket (createProcess followProcess) stop $ \(_, out, _, _) ->
    case out of
        Nothing -> ioError (userError "playerctl --follow: no stdout")
        Just h  -> hSetBuffering h LineBuffering >> k h
  where
    stop :: (a, Maybe Handle, b, ProcessHandle) -> IO ()
    stop (_, out, _, ph) = do
        terminateProcess ph
        maybe (pure ()) hClose out
        void (waitForProcess ph)

parseTrack :: String -> Maybe Track
parseTrack line = case splitOn fieldSep line of
    [status, title, artist] | not (null status) -> Just (Track status title artist)
    _                                           -> Nothing

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (field, [])         -> [field]
    (field, _ : remain) -> field : splitOn sep remain


render :: Int -> Track -> String
render now track = concat
    [ capLeft, previous, transport (trackStatus track), next
    , " ", text (scrollTitle now (trackTitle track))
    , " ", xmobarColor subtleFg bubbleBgSpec "-"
    , " ", text (trackArtist track)
    , capRight
    ]
  where
    text = xmobarColor bubbleFg bubbleBgSpec

capLeft, capRight :: String
capLeft  = xmobarColor bubbleBgSpec sepBgSpec (xmobarFont nerdFontIdx "\xe0b6")
capRight = xmobarColor bubbleBgSpec barBg     (xmobarFont nerdFontIdx "\xe0b4 ")

previous, next :: String
previous = button "playerctl previous" (glyph dimFg "\xf9ad")
next     = button "playerctl next"     (glyph dimFg "\xf9ac")

transport :: String -> String
transport status
    | isPlaying = wrapped pause (glyph okFg    " \xf28b ")
    | otherwise = wrapped play  (glyph alertFg " \xf144 ")
  where
    isPlaying     = "Playing" `isPrefixOf` status
    wrapped act b = mouse 3 stop (mouse 2 restart (button act b))

    play    = "playerctl --player=" <> player <> " play"
    pause   = "playerctl --player=" <> player <> " pause"
    stop    = "playerctl stop"
    restart = "playerctl --player=" <> player <> " -a pause && "
           <> "playerctl --player=" <> player <> " play"

glyph :: String -> String -> String
glyph colour g = xmobarColor colour bubbleBgSpec (xmobarFont nerdFontIdx g)

button :: String -> String -> String
button command body = "<action=" <> command <> ">" <> body <> "</action>"

mouse :: Int -> String -> String -> String
mouse n command body =
    "<action=`" <> command <> "` button=" <> show n <> ">" <> body <> "</action>"

scrollTitle :: Int -> String -> String
scrollTitle now title
    | len <= scrollLength = title
    | otherwise           = take scrollLength (drop offset padded)
  where
    len    = length title
    offset = now `mod` (len + 3)
    padded = title <> "   " <> title