module Theme.Scale ( uiScale, scaled, setCursorEnv ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Foldable          (traverse_)
import           System.Environment     (setEnv)
import           Text.Read              (readMaybe)
import           Theme.Xresources       (xprop)

uiScale :: Double
uiScale = case readMaybe (xprop "Xft.dpi") :: Maybe Double of
    Just dpi | dpi > 0 -> dpi / 96
    _                  -> 1

scaled :: Integral a => Double -> a
scaled n = round (n * uiScale)

setCursorEnv :: MonadIO m => m ()
setCursorEnv = liftIO $ traverse_ export
    [ ("XCURSOR_THEME", "Xcursor.theme")
    , ("XCURSOR_SIZE",  "Xcursor.size")
    ]
  where
    export (var, res) = unless (null value) (setEnv var value)
      where value = xprop res