{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedWindows
import Data.Word
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map                      as M
import qualified Data.List                      as L
import           Data.Time.Clock (getCurrentTime)
import           Data.Monoid
import           XMonad
import           XMonad.Util.Run
import           XMonad.Hooks.ManageDocks
import           XMonad.Actions.CycleWS
import           XMonad.Config
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.FloatSnap
import           XMonad.Actions.UpdatePointer
import           XMonad.Util.WorkspaceCompare (getSortByIndex)
import           XMonad.Actions.GridSelect
import           XMonad.Hooks.EwmhDesktops      (ewmh, fullscreenEventHook, ewmhFullscreen)
-- import qualified XMonad.Hooks.ICCCMFocus       as ICCCMFocus
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.DynamicLog
import           XMonad.Util.Themes
import           XMonad.Layout.Decoration
import           XMonad.Layout.Grid
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Decoration
import           XMonad.Layout.Cross
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Simplest
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.NoBorders
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Maximize
import           XMonad.Layout.SimpleDecoration

import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Input as In
import XMonad.Prompt as XP
import XMonad.Prompt.ConfirmPrompt as CP
import XMonad.Prompt.RunOrRaise as ROR
import XMonad.Prompt.Window as PW
import XMonad.Prompt.Layout as PL

import XMonad.Layout.Tabbed          as T
import           XMonad.Layout.TwoPane
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run                ( safeSpawn )
import           XMonad.Actions.GroupNavigation
import           XMonad.Layout.WindowNavigation
import           XMonad.Layout.PerWorkspace
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.ServerMode
import qualified XMonad.Util.ExtensibleState as XS

tabfont = "xft:JetBrains Mono:bold:size=12"
promptfont = "xft:JetBrains Mono:bold:size=13"
color1 :: String
color1 = "#758b8b" -- active background
color2 :: String
color2 = "#5d5b6a" -- inactive background
color3 :: String
color3 = "#f5cdaa" -- active text
color4 :: String
color4 = "#f5cdaa" -- inactive text
colorblack :: String
colorblack = "#000000"
colorgrey :: String
colorgrey = "#2b2f38"
colorwhite :: String
colorwhite = "#00fafa"
colorBorder :: String
colorBorder = "#ccaa22"
colorred :: String
colorred = "#ff0000"
up :: X()
up = updatePointer (0.5, 0.5) (0, 0)
myTerminal :: String
myTerminal = "alacritty"

main = xmonad $ ewmh $ docks $ ewmhFullscreen $ def
      { workspaces         =  [ "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        , manageHook = manageSpawn <+> myManageHook <+> manageDocks <+> manageHook def
        , layoutHook         = myLayoutHook
        , handleEventHook    = serverModeEventHook' commands <+> handleEventHook def
        , modMask            = mod4Mask
        -- , logHook            = ICCCMFocus.takeTopFocus <+> historyHook
        , startupHook        = startupApp
        , terminal           = myTerminal
        , keys               = myKeys
        , borderWidth        = 2
        , normalBorderColor  = "#000000"
        , focusedBorderColor = "#000000"
        , focusFollowsMouse  = False }
myKeys conf@XConfig { XMonad.modMask = modm } =

    M.fromList
        $  [
           ((modm, xK_Left)      , XMonad.Actions.CycleWS.moveTo Prev (Not emptyWS))
           , ((modm, xK_Right)      , XMonad.Actions.CycleWS.moveTo Next (Not emptyWS))
           , ((modm .|. shiftMask, xK_Left)      , moveNonEmptyAndJump Prev)
           , ((modm .|. shiftMask, xK_Down)      , XMonad.Actions.CycleWS.moveTo Next emptyWS)
           , ((modm .|. shiftMask, xK_Right)      , moveNonEmptyAndJump Next)
           , ((modm, xK_n)      , moveNonEmptyAndJump Next)
           , ((modm .|. shiftMask, xK_Up)      , moveEmptyAndJump)
           , ((modm, xK_i)      , moveEmptyAndJump)
           , ((mod1Mask, xK_F4), kill)
           , ( (modm, xK_Return), windows W.swapMaster)
           , ( (modm, xK_f), sendMessage ToggleStruts)
           , ( (modm, xK_b), withFocused $ windows . W.sink)
           , ((modm, xK_o), nextScreen)
           , ((modm, xK_space), sendMessage NextLayout)
           , ((modm .|. shiftMask, xK_space), sendMessage FirstLayout)
           , ((modm .|. shiftMask, xK_o)  , shiftAndSwapScreen)
           , ((modm .|. shiftMask, xK_Return)  , spawn myTerminal)
           , ((modm .|. shiftMask, xK_j), windows W.swapUp)   -- Move up in stack
           , ((modm, xK_j), windows W.focusUp)-- Focus down in stack
           , ((modm, xK_Up), windows W.focusUp)-- Focus down in stack
           , ((modm, xK_Down), windows W.swapUp)-- Focus down in stack
           , ((modm, xK_k), windows W.focusDown)  -- Focus up in stack
           , ((modm .|. shiftMask, xK_k), windows W.swapDown) -- Move down in stack
           , ((modm, xK_h), XMonad.Actions.CycleWS.moveTo Prev (Not emptyWS)) -- Move down in stack
           , ((modm, xK_l), XMonad.Actions.CycleWS.moveTo Next (Not emptyWS)) -- Move down in stack
           , ((mod1Mask, xK_Tab), nextMatch Forward isOnAnyVisibleWS) -- Move down in stack
            , ((mod1Mask .|. shiftMask, xK_Tab), nextMatch Backward isOnAnyVisibleWS)
           -- M-C-hjkl
           , ((modm .|. controlMask, xK_Left), sendMessage Shrink)       -- Resize Master split
           , ((modm .|. controlMask, xK_Up), sendMessage MirrorShrink) -- Resive vertically
           , ((modm .|. controlMask, xK_Down), sendMessage MirrorExpand) -- Resive vertically
           , ((modm .|. controlMask, xK_Right), sendMessage Expand)       -- Resize Master split
           , ((modm .|. shiftMask, xK_u), withFocused (sendMessage . UnMerge)) -- Merge Down
           , ((0, 0x1008ff13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
           , ((0, 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
           , ((modm, xK_semicolon), withFocused (sendMessage . maximizeRestore))
          --  , ((0, 0xff61), jumpToBack "5")
           , ((modm, xK_m), spawn "~/.local/bin/my-app-menu.sh")
          --  , ((modm .|. shiftMask, xK_m), layoutChooser)
           ]
        ++
        [((m .|. modm, k), windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
        ++
        [ ( (m .|. modm, key)
             , screenWorkspace sc >>= flip whenJust (windows . f)
             )
           | (key, sc) <- zip [xK_w, xK_e] [0 ..]
           , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
           ]

moveEmptyAndJump = do
  nextWSId <- findWorkspace getSortByIndex Next emptyWS 1
  windows $ W.shift nextWSId
  windows $ W.view nextWSId

moveNonEmptyAndJump nextprev = do
  nextWSId <- findWorkspace getSortByIndex nextprev (Not emptyWS) 1
  windows $ W.shift nextWSId
  windows $ W.view nextWSId

jumpTo :: String -> WindowSet -> WindowSet
jumpTo wId s = W.view wId (W.shift wId s)

jumpTo' :: String -> WindowSet -> WindowSet
jumpTo' wId s = if (W.tag $ W.workspace $ W.current s) == wId then
  W.focusUp s
  else
    W.view wId s

type TargetWindow = String
type PreviousWindow = String
data MyWindowState = MyWindowState [(TargetWindow, PreviousWindow)] deriving Typeable
instance ExtensionClass MyWindowState where
  initialValue = MyWindowState []

jumpToBack :: String -> X ()
jumpToBack wId = withWindowSet $ \cws -> do -- cws = current window set
  let cTag = W.tag $ W.workspace $ W.current cws
  io $ putStrLn ("Current ws: " ++ cTag)
  (MyWindowState mws) <- XS.get
  io $ putStrLn "bakker"
  if wId /= cTag then do
    io $ putStrLn "jump"
    XS.put $ MyWindowState [(wId :: TargetWindow, cTag :: PreviousWindow)]
    jump
    else do
      case lookup (wId :: TargetWindow) mws of
        Just prevWSId -> goBack prevWSId
        Nothing -> return ()
  where
    jump :: X ()
    jump = windows $ W.view wId
    goBack :: PreviousWindow -> X ()
    goBack prevWId = do
      trace ("goBack: " ++ prevWId)
      windows $ W.view prevWId
  
                
mainMenuCommands :: [(String, String)]
mainMenuCommands = [("Run ...", "budgie-run-dialog"), ("Browser", "firefox"), ("File Manager", "nautilus"), ("Security box", "keepass2"), ("Calculator", "gnome-calculator")]
mainMenuCommands' = foldr createCommand [] mainMenuCommands
  where
    createCommand (name, cmd) result = [(name, (name, baseCommand cmd))] ++ result
    baseCommand cmd = spawn cmd
    
mainMenu' :: X ()
mainMenu' = do
  result <- gridselect myGSConfig4 mainMenuCommands'
  case result of
    Nothing -> return ()
    Just (_, command) -> command
  
myGSConfig4 = def { gs_cellheight   = 30
                   , gs_cellwidth    = 400
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_colorizer = (\item@(itemName, _) p1 -> stringColorizer itemName p1)
                   , gs_originFractY = 0.5
                   , gs_font = tabfont }
              
testType :: (Monad m) => Int -> m ()
testType a = undefined
  
commands :: X [(String, X ())]
commands = return $ [("LayoutChooser", layoutChooser), ("VolumeAndBrightness", volumeAndBrightnessControl)]

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
  d   <- openDisplay ""
  rw  <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
                  setEventType e clientMessage
                  setClientMessageEvent e rw a 32 m currentTime
                  sendEvent d rw False structureNotifyMask e
                  sync d False

jumpToOrSwap :: String -> WindowSet -> WindowSet
jumpToOrSwap tag' s =
  if (W.tag $ W.workspace $ W.current s) == tag' then
    W.swapUp s
  else moveAndJump s
  where
    moveAndJump s' = do
      let s'' = W.shift tag' s'
      W.view tag' s''

startupApp :: X ()
startupApp = do
  spawn "~/.local/bin/xmonad-startup.sh"

myLayout =
     T.addTabsBottomAlways T.shrinkText myTabConfig $
     subLayout [0] (Simplest) $
     boringWindows $
     ResizableTall 1 (3/100) (1/2) []

myTabConfig2 = def { decoHeight = 0 }
titleConfig = def { activeBorderWidth = 0, inactiveBorderWidth = 0, urgentBorderWidth = 0, decoWidth = 0, decoHeight = 5, activeColor = "#808aa9", activeTextColor = "#000000", inactiveColor = "#292e38", inactiveTextColor = "#ffffff" }
myTabConfig = def { fontName = tabfont, decoHeight = 0, inactiveColor = "#292e38", activeColor = "#808aa9", inactiveTextColor = "white", activeTextColor = "black", activeBorderColor = "black", inactiveBorderColor = "black", activeBorderWidth = 2, inactiveBorderWidth = 1 }
myTabbed = T.tabbedBottomAlways T.shrinkText myTabConfig
myLayoutHook = avoidStruts $ windowNavigation $ maximize $ (simpleDeco (MyShrink) titleConfig) $ myLayout ||| myTabbed 

floatClassname = ["Xmessage", "File-roller", "yad", "Yad", "gmrun", "pavucontrol", "xarchiver"]
floatTitle = ["Save File", "gmrun"]
sendTo9Classname = ["gnome-calculator", "keepass2", "Keepass2"]

data MyShrink = MyShrink deriving (Show, Read)
instance Shrinker MyShrink where
  shrinkIt _ _ = []

myManageHook :: ManageHook
myManageHook = composeAll
            . concat
            $ [ [isDialog --> doCenterFloat]
            , [isFullscreen --> doFullFloat]
              , [className =? c --> doCenterFloat | c <- floatClassname ]
              , [title =? t --> doCenterFloat | t <- floatTitle ]
              , [winCheck t' --> doF (jumpTo "9") | t' <- sendTo9Classname ]
              -- , [liftX (return True) -?> doF (io $ putStrLn $ "calssName:" ++ className)]
              ]

winCheck x = title =? x <||> appName =? x <||> className =? x

layoutPromptStyle = def { gs_cellheight   = 30
                   , gs_cellwidth    = 400
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_colorizer = (\_ isActive -> if isActive then return ("white", "black") else return ("black", "white"))
                   , gs_font = tabfont }

actions = [("+ Master +", sendMessage (IncMasterN 1))
       ,("- Master -", sendMessage (IncMasterN (-1)))
       ,("Group Left", sendMessage $ pullGroup L)
       ,("Group Right", sendMessage $ pullGroup R)
       ,("Group Up", sendMessage $ pullGroup U)
       ,("Group Down", sendMessage $ pullGroup D)
       ,("Move Down", windows W.swapDown)
       ,("Move Up", windows W.swapUp)
       ,("Shrink", sendMessage Shrink)       -- Resize Master split
       ,("Mirror Shrink", sendMessage MirrorShrink) -- Resive vertically
       ,("Mirror Expand", sendMessage MirrorExpand) -- Resive vertically
       ,("Expand", sendMessage Expand)       -- Resize Master split
       ,("Merge Up", withFocused (sendMessage . mergeDir W.focusUp')) -- Merge up
       ,("Tab Up", onGroup W.focusUp') -- Switch focus up tabs
       ,("Tab Down", onGroup W.focusDown') -- Switch focus down tabs
       ,("Merge Down", withFocused (sendMessage . mergeDir W.focusDown')) -- Merge Down
       ,("UnMerge", withFocused (sendMessage . UnMerge))] -- Merge Down

layoutChooser :: X ()
layoutChooser = do
 a <- gridselect layoutPromptStyle actions
 case a of
   Nothing -> return ()
   Just a' -> do
               a'
               trace "xmonad sendCommand before"
               io $ sendCommand "XMONAD_COMMAND" "1"

vAbActions = [("Volume: UP", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
             ,("Volume: DOWN", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
             ,("Light: UP", spawn "brightness.sh up")
             ,("Light: DOWN", spawn "brightness.sh down")
             ,("Volume: MUTE", spawn "pactl set-sink-mute @DEFAULT_SINK@ 1")
             ,("Volume: UNMUTE", spawn "pactl set-sink-mute @DEFAULT_SINK@ 0")]
volumeAndBrightnessControl :: X ()
volumeAndBrightnessControl = do
  res <- gridselect layoutPromptStyle vAbActions
  case res of
    Nothing -> return ()
    Just res' -> do
      res'
      io $ sendCommand "XMONAD_COMMAND" "2"

shiftAndSwapScreen :: X ()
shiftAndSwapScreen = do
    shiftNextScreen
    nextScreen

promptCenter = XP.def
   { XP.font = promptfont
   , XP.height = 32
   , XP.position = XP.CenteredAt 0.4 0.7
   , XP.borderColor = "#AAAAAA"
   , XP.fgColor = "#ffffff"
   , XP.bgColor = "#494949"
   , XP.fgHLight = "#000000"
   , XP.bgHLight = "#ccaa22"
   , XP.promptBorderWidth = 2
   , XP.searchPredicate = fuzzyMatch
   , XP.maxComplRows = Just 10
   , XP.autoComplete = Just 100
   , XP.historySize = 100
   , XP.promptKeymap = XP.vimLikeXPKeymap
   , XP.alwaysHighlight = True
   , XP.sorter = fuzzySort
   }

promptFuzzy = XP.def
  {  XP.searchPredicate = fuzzyMatch
   , XP.sorter = fuzzySort
  }

