module Screens
    ( barScreen
    , barScreenRect
    , otherScreen
    , screenRectOf
    ) where

import           XMonad
import           XMonad.Prelude  (fi, find, listToMaybe, sortOn)
import qualified XMonad.StackSet as W

screenList :: X [(ScreenId, Rectangle)]
screenList = do
    ss <- gets (W.screens . windowset)
    pure [ (W.screen s, screenRect (W.screenDetail s)) | s <- ss ]

barScreenEntry :: X (Maybe (ScreenId, Rectangle))
barScreenEntry = listToMaybe . sortOn rank <$> screenList
  where
    -- Negated so that sortOn puts the largest first without needing Data.Ord.
    rank :: (ScreenId, Rectangle) -> (Int, Int, ScreenId)
    rank (sid, r) = (negate (fi (rect_width r)), negate (fi (rect_height r)), sid)

barScreen :: X (Maybe ScreenId)
barScreen = fmap fst <$> barScreenEntry

barScreenRect :: X (Maybe Rectangle)
barScreenRect = fmap snd <$> barScreenEntry

otherScreen :: X (Maybe ScreenId)
otherScreen = do
    primary <- barScreen
    entries <- screenList
    pure $ fst <$> find ((/= primary) . Just . fst) entries

screenRectOf :: ScreenId -> X (Maybe Rectangle)
screenRectOf sid = fmap snd . find ((== sid) . fst) <$> screenList