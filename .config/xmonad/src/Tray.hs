module Tray
    ( trayCmd
    , trayQuery
    , trayEventHook
    , syncTray
    ) where

import           Screens                (barScreenRect)
import           Theme.Bar
import           Theme.Theme            (basebg)
import           XMonad
import           XMonad.Hooks.StatusBar (xmonadPropLog')
import           XMonad.Prelude
import qualified XMonad.Util.Hacks      as Hacks

trayCmd :: String
trayCmd = unwords
    [ "stalonetray"
    , "--geometry",     "1x1-" <> show trayMargin <> "+" <> show trayMargin
    , "--max-geometry", show trayMaxIcons <> "x1"
    , "--transparent"
    , "--tint-color",   "'" <> basebg <> "'"
    , "--tint-level",   "255"
    , "--grow-gravity", "NE"
    , "--icon-gravity", "NE"
    , "--icon-size",    show trayIconSize
    , "--sticky"
    , "--window-type",  "dock"
    , "--window-strut", "none"
    , "--skip-taskbar"
      -- Clamp every icon to --icon-size. snixembed sizes an XEmbed proxy from
      -- the StatusNotifierItem's properties, and when a property fetch fails
      -- it leaves the proxy at GTK's default 200x200 with no size hints at
      -- all. vesktop triggers exactly that: its SNI item answers
      -- @Get IconName@ with a D-Bus error rather than an empty string, so its
      -- proxy arrives unsized, stalonetray gives it a 200x200 slot, and that
      -- slot overflows the tray and displaces every other icon.
    , "--kludges",      "force_icons_size"
    ]

trayQuery :: Query Bool
trayQuery = className =? "stalonetray"

panelQuery :: Query Bool
panelQuery = appName =? "xmobar" <||> className =? "xmobar"

trayEventHook :: Event -> X All
trayEventHook ev =
       Hacks.trayPaddingXmobarEventHook trayQuery trayPadProp ev
    <> repinOnConfigure ev
    <> restackOnEvent ev

trayPosition :: Dimension -> X (Maybe (Position, Position))
trayPosition w = fmap place <$> barScreenRect
  where
    place r = ( rect_x r + fi (rect_width r) - fi trayMargin - fi w
              , rect_y r + fi trayMargin
              )

repinOnConfigure :: Event -> X All
repinOnConfigure ConfigureEvent{ ev_window = w, ev_x = x, ev_y = y, ev_width = width } = do
    whenX (runQuery trayQuery w) $ do
        mpos <- trayPosition (fi width)
        whenJust mpos $ \(wantX, wantY) ->
            when (fi x /= wantX || fi y /= wantY) $
                withDisplay $ \dpy -> io $ moveWindow dpy w wantX wantY
    mempty
repinOnConfigure _ = mempty

restackOnEvent :: Event -> X All
restackOnEvent ev = do
    case ev of
        ConfigureEvent{ ev_window = w } -> go w
        MapNotifyEvent{ ev_window = w } -> go w
        _                               -> pure ()
    mempty
  where
    go w = whenX (runQuery (trayQuery <||> panelQuery) w) lowerDocks

lowerDocks :: X ()
lowerDocks = withDisplay $ \dpy -> do
    root       <- asks theRoot
    (_, _, ws) <- io $ queryTree dpy root   -- bottom to top
    panels     <- filterM (runQuery panelQuery) ws
    trays      <- filterM (runQuery trayQuery)  ws
    let docks = panels <> trays
    unless (null docks || take (length docks) ws == docks) $
        mapM_ (io . lowerWindow dpy) (reverse docks)

setTrayPad :: Int -> X ()
setTrayPad w = xmonadPropLog' trayPadProp ("<hspace=" <> show w <> "/>")

syncTray :: X ()
syncTray = lowerDocks >> withDisplay (\dpy -> do
    root       <- asks theRoot
    (_, _, ws) <- io $ queryTree dpy root
    trays      <- filterM (runQuery trayQuery) ws
    forM_ trays $ \w -> do
        mwa <- safeGetWindowAttributes w
        whenJust mwa $ \wa -> do
            let width = fi (wa_width wa)
            setTrayPad width
            mpos <- trayPosition (fi width)
            whenJust mpos $ \(wantX, wantY) ->
                when (fi (wa_x wa) /= wantX || fi (wa_y wa) /= wantY) $
                    io $ moveWindow dpy w wantX wantY)