-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile

import XMonad
import XMonad.ManageHook

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.OnScreen
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Submap
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer

-- Hooks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.UrgencyHook hiding (Never)

-- Layouts
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.Minimize
import XMonad.Layout.BoringWindows

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.AppendFile

-- Utils
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)

import Control.Monad
import System.IO
import System.IO.Unsafe
import Data.List
import Data.Ratio ((%))

import qualified XMonad.Layout.ToggleLayouts as Tog
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Stackset as W
import qualified Data.Map as M

myTerminal = "urxvt"
myFocusFollowsMouse = True
myBorderWidth = 3
myModMask = mod4Mask
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#ff0000"
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

-- Fonts, icons, etc
myFont = "-*-terminus-medium-*-*-*-12-120-75-75-*-*-iso8859-*"
myFontB = "-*-terminus-bold-*-*-*-16-160-72-72-*-*-iso8859-*"
myShell = "zsh"
myIconDir = "~/.dzen/icons/"

-- GSConfig optiones:
myGSConfig = defaultGSConfig
    {
    gs_cellheight = 50,
    gs_cellwidth = 250,
    gs_cellpadding = 10,
    gs_font = "" ++ myFont ++ ""
    }

-- XPConfig optiones:
myXPConfig = defaultXPConfig
    {
    font = "" ++ myFont ++ "",
    fgColor = "" ++ myNormalFGColor ++ "",
    bgColor = "" ++ myNormalBGColor ++ "",
    fgHLight = "" ++ myNormalFGColor ++ "",
    bgHLight = "" ++ myUrgentBGColor ++ "",
    borderColor = "" ++ myFocusedBorderColor ++ "",
    promptBorderWidth = 1,
    position = Bottom,
    height = 16,
    historySize = 100
    }

-- Theme optiones:
myTheme = defaultTheme
    {
    activeColor = "" ++ myFocusedBGColor ++ "",
    inactiveColor = "" ++ myDzenBGColor ++ "",
    urgentColor = "" ++ myUrgentBGColor ++ "",
    activeBorderColor = "" ++ myFocusedBorderColor ++ "",
    inactiveBorderColor = "" ++ myNormalBorderColor ++ "",
    urgentBorderColor = "" ++ myNormalBorderColor ++ "",
    activeTextColor = "" ++ myFocusedFGColor ++ "",
    inactiveTextColor = "" ++ myDzenFGColor ++ "",
    urgentTextColor = "" ++ myUrgentFGColor ++ "",
    font = "" ++ myFont ++ "",
    }

-- Statusbar optiones:
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '900' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar = "conky -c .conkytop | dzen2 -x '900' -y '0' -h '16' -w '124' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '600' -h '16' -w '1000' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myHDDBar = "conky -c .conkyhdd | dzen2 -x '1000' -y '600' -h '16' -w '24' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

-- Urgency hint optiones:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
    args = ["-x", "0", "-y", "600", "-h", "16", "-w", "1024", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""]
    }

-- Layouts:
myLayout = avoidStruts $ layoutHints $ onWorkspace "1" (resizableTile ||| Mirror resizableTile) $ onWorkspace "6" gimpLayout $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile)
    where
    resizableTile = ResizableTall nmaster delta ratio []
    tabbedLayout = tabbedBottomAlways shrinkText myTheme
    gimpLayout = combineTwoP (TwoPane 0.04 0.82) (tabbedLayout) (Full) (Not (Role "gimp-toolbox"))
    nmaster = 1
    ratio = toRational (2 / (1 + sqrt(5) :: Double))
    delta = 3 / 100

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),

    -- launch dmenu
    --((modm, xK_p), spawn "dmenu_run"),

    -- close focused window
    ((modm .|. shiftMask, xK_c), kill),

    -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),

    --  Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),

    -- Move focus to the next window
    ((modm, xK_j), windows W.focusDown),

    -- Move focus to the previous window
    ((modm, xK_k), windows W.focusUp),

    -- Move focus to the master window
    ((modm, xK_m), windows W.focusMaster),

    -- Swap the focused window and the master window
    ((modm, xK_Return), windows W.swapMaster),

    -- Swap the focused window with the next window
    ((modm .|. shiftMask, xK_j), windows W.swapDown),

    -- Swap the focused window with the previous window
    ((modm .|. shiftMask, xK_k), windows W.swapUp),

    -- Shrink the master area
    ((modm, xK_h), sendMessage Shrink),

    -- Expand the master area
    ((modm, xK_l), sendMessage Expand),

    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),

    -- Increment the number of windows in the master area
    ((modm, xK_comma), sendMessage (IncMasterN 1)),

    -- Deincrement the number of windows in the master area
    ((modm, xK_period), sendMessage (IncMasterN (-1))),

    -- Quit xmonad
    ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)),

    -- Restart xmonad
    ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    ]

    ++

    [
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

    ++

    [
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),

    -- mod-button3, Set the window to floating mode and resize by dragging
    ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

-- Xmonad
main = do
    dzen <- spawnPipe myStatusBar
    conkytop <- spawnPipe myTopBar
    conkympd <- spawnPipe myMPDBar
    conkyhdd <- spawnPipe myHDDBar
    xmonad $ myUrgencyHook $ defaultConfig
        {
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys = myKeys,
        mouseBindings = myMouseBindings,

        layoutHook = myLayout,
        manageHook = myManageHook <+> manageDocks <+> dynamicMasterHook,
        handleHookEvent = myEventHook,
        logHook = dynamicLogWithPP $ myDzenPP dzen,
        startupHook = myStartupHook
        }
