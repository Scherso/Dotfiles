module Theme.Xresources (xprop) where

import           Data.Bifunctor   (bimap)
import           Data.Char        (isSpace)
import           Data.List        (dropWhileEnd, elemIndex, find)
import           Data.Maybe       (catMaybes, fromMaybe)
import           System.IO.Unsafe (unsafePerformIO)
import           XMonad.Util.Run  (runProcessWithInput)

-- xrdb output captured once at startup and shared across all xprop lookups.
{-# NOINLINE xresources #-}
xresources :: String
xresources = unsafePerformIO $ runProcessWithInput "xrdb" ["-query"] ""

xprop :: String -> String
xprop key = fromMaybe "" $ findValue key xresources

findValue :: String -> String -> Maybe String
findValue key xres = snd <$> find ((== key) . fst) (catMaybes $ splitAtColon <$> lines xres)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim (trim . drop 1) $ splitAt idx str

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
