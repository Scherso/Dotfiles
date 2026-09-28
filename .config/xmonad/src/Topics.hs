module Topics
    (
      topicItems
    , myTopics
    , myTopicConfig
    , secondaryTopic
    , goto
    , shiftToTopic
    , spawnShell
    , spawnShellIn
    , promptedGoto
    , promptedShift
    , toggleTopic
    , runTopicAction
    , topicHistoryHook
    , checkTopics
    , pruneStaleTopics
    ) where

import           Settings                         (myBrowser, myTerminal, myOffice)
import           Theme.Bar                        (barBg, barFont, barHeight, borderCol,
                                                   bubbleBg, bubbleFg)
import           Theme.Scale                      (scaled)
import           XMonad
import           XMonad.Actions.DynamicWorkspaces (removeEmptyWorkspaceByTag)
import           XMonad.Actions.TopicSpace
import           XMonad.Prompt                    (XPConfig (..), XPPosition (Top))
import           XMonad.Prompt.Workspace          (workspacePrompt)
import qualified XMonad.StackSet                  as W
import           XMonad.Util.NamedScratchpad      (scratchpadWorkspaceTag)

topicItems :: [TopicItem]
topicItems =
    [ inHome   "web"                    (spawn myBrowser)
    , TI       "dev"  "Projects"        spawnShell
    , TI       "xmo"  ".config/xmonad"  spawnShell
    , inHome   "sys"                    spawnShell
    , noAction "chat" "."
    , noAction "med"  "."
    , noAction "game" "."
    , TI       "doc"  "."               (spawn myOffice)
    , noAction "misc" "."
    ]

myTopics :: [Topic]
myTopics = topicNames topicItems

secondaryTopic :: Topic
secondaryTopic = "chat"

myTopicConfig :: TopicConfig
myTopicConfig = def
    { topicDirs          = tiDirs    topicItems
    , topicActions       = tiActions topicItems
    , defaultTopicAction = const (pure ())   -- an unlisted topic opens nothing
    , defaultTopic       = "web"
    }

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

shiftToTopic :: Topic -> X ()
shiftToTopic = windows . W.shift

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn (myTerminal <> " --working-directory ~/" <> dir)

promptedGoto, promptedShift :: X ()
promptedGoto  = workspacePrompt promptTheme goto
promptedShift = workspacePrompt promptTheme shiftToTopic

toggleTopic :: X ()
toggleTopic = switchNthLastFocusedByScreen myTopicConfig 1

runTopicAction :: X ()
runTopicAction = currentTopicAction myTopicConfig

topicHistoryHook :: X ()
topicHistoryHook = workspaceHistoryHookExclude [scratchpadWorkspaceTag]

checkTopics :: X ()
checkTopics = io (checkTopicConfig myTopics myTopicConfig)

pruneStaleTopics :: X ()
pruneStaleTopics = do
    tags <- gets (map W.tag . W.workspaces . windowset)
    mapM_ removeEmptyWorkspaceByTag (filter stale tags)
  where
    stale t = t `notElem` myTopics && t /= scratchpadWorkspaceTag

promptTheme :: XPConfig
promptTheme = def
    { font              = barFont
    , bgColor           = barBg
    , fgColor           = bubbleFg
    , bgHLight          = bubbleBg
    , fgHLight          = bubbleFg
    , borderColor       = borderCol
    , promptBorderWidth = scaled 1
    , position          = Top
    , height            = fromIntegral barHeight
    }