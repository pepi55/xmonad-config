-- File : ~/.xmonad/xmonad.hs
-- Auth : Pepi55 <pepi55@live.com>
-- Desc : My modified XMonad config

-- Options
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}

-- Modules
import XMonad
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MagicFocus
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowSwitcherNavigation
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutBuilder
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Man
import XMonad.Util.Timer
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.FloatKeys

import Data.Monoid
import Data.List

import System.Exit
import System.IO

import Graphics.X11.Xinerama
import Graphics.X11.ExtraTypes.XF86

import Control.Concurrent
import Control.Applicative
import Control.Exception as E

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Stackset as W
import qualified Data.Map as M

myTerminal = "/usr/bin/urxvt"
myFocusFollowsMouse = True
myBorderWidth = 3
myModMask = mod4Mask

-- Main
main :: IO ()
main = do
    r <- getScreenRes ":0" 0
    topLeftBar <- dzenSpawnPipe $ dzenTopLeftFlags r
    topRightBar <- dzenSpawnPipe $ dzenTopRightFlags r
    botLeftBar <- dzenSpawnPipe $ dzenBotLeftFlags r
    botRightBar <- dzenSpawnPipe $ dzenBotRightFlags r
    xmonad $ myUrgencyHook defaultConfig
        {
        terminal = myTerminal,
        modMask = myModMask,
        focusFollowsMouse = myFocusFollowsMouse
        borderWidth = myBorderWidth,
        normalBorderColor = colorBlackAlt,
        focusedBorderColor = colorWhiteAlt2,
        workspaces = myWorkspaces,

        startupHook = myStartupHook,
        handleHookEvent = myEventHook,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,

        logHook =
            myTopLeftLogHook topLeftBar <+>
            myTopRightLogHook topRightBar <+>
            myBotLeftLogHook botLeftBar <+>
            myBotRightLogHook botRightBar <+>
            ewmhDesktopsLogHook >>
            setWMName "LG3D",

        keys = myKeys,
        mouseBindings = myMouseBindings,
        }

-- Look and feel
dzenFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*" --"-*-terminus-medium-*-*-*-12-120-75-75-*-*-iso8859-*"
colorBlack = "#020202"
colorBlackAlt = "#1C1C1C"

colorGray = "#444444"
colorGrayAlt = "#101010"
colorGrayAlt2 = "#404040"
colorGrayAlt3 = "#252525"

colorWhite = "#A9A6AF"
colorWhiteAlt = "#9D9D9D"
colorWhiteAlt2 = "#B5B3B3"
colorWhiteAlt3 = "#707070"

colorMagenta = "#8E82A2"

colorBlue = "#44AACC"
colorBlueAlt = "#3955C4"

colorRed = "#F7A16E"
colorRedAlt = "#E0105F"

colorGreen = "#66FF66"
colorGreenAlt = "#558965"

boxLeftIcon = "~/.xbm_icons/boxleft.xbm"
boxLeftIcon2 = "~/.xbm_icons/boxleft2.xbm"
boxRightIcon = "~/.xbm_icons/boxright.xbm"

panelHeight = 16
boxHeight = 14

topPanelSepPos = 950
botPanelSepPos = 450

-- Title theme
myTitleTheme :: Theme
myTitleTheme = defaultTheme
    {
    fontName = dzenFont,
    inactiveBorderColor = colorGrayAlt2,
    inactiveColor = colorGrayAlt3,
    inactiveTextColor = colorWhiteAlt3,
    activeBorderColor = colorGrayAlt2,
    activeColor = colorGrayAlt2,
    activeTextColor = colorWhiteAlt2,
    urgentBorderColor = colorGray,
    urgentTextColor = colorGreen,
    decoHeight = 14
    }

-- Prompt theme
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    {
    font = dzenFont,
    bgColor = colorBlack,
    fgColor = colorWhite,
    bgHLight = colorBlue,
    fgHLight = colorBlack,
    borderColor = colorGrayAlt,
    promptBorderWidth = 1,
    height = panelHeight,
    position = Top,
    historySize = 1000,
    historyFilter = deleteConsecutive,
    autoComplete = Nothing
    }

-- Gridselect colorscheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
    (0x00, 0x00, 0x00) -- Lowest inactive bg
    (0x1C, 0x1C, 0x1C) -- Highest inactive bg
    (0x44, 0xAA, 0xCC) -- Active bg
    (0xBB, 0xBB, 0xBB) -- Inactive fg
    (0x00, 0x00, 0x00) -- Active fg

-- Gridselect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
    {
    gs_cellheight = 50,
    gs_cellwidth = 200,
    gs_cellpadding = 10,
    gs_font = dzenFont
    }

-- Flash text config
myTextConfig :: ShowTextConfig
myTextConfig = STC
    {
    st_font = dzenFont,
    st_bg = colorBlack,
    st_fg = colorWhite
    }

-- Dzen logger box printing
gray2BoxPP :: BoxPP
gray2BoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorGray,
    boxColorBPP = colorGrayAlt,
    leftIconBPP = boxLeftIcon2,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

blueBoxPP :: BoxPP
blueBoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorBlue,
    boxColorBPP = colorGrayAlt,
    leftIconBPP = boxLeftIcon,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

blue2BoxPP :: BoxPP
blue2BoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorBlue,
    boxColorBPP = colorGrayAlt,
    leftIconBPP = boxLeftIcon2,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

blackBoxPP :: BoxPP
blackBoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorBlack,
    boxColorBPP = colorGrayAlt,
    leftIconBPP = boxLeftIcon,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

whiteBoxPP :: BoxPP
whiteBoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorWhiteAlt,
    boxColorBPP = colorGrayAlt,
    leftIconBPP = boxLeftIcon,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

white2BBoxPP :: BoxPP
white2BBoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorBlack,
    boxColorBPP = colorWhiteAlt,
    leftIconBPP = boxLeftIcon2,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

blue2BBoxPP :: BoxPP -- Current workspace
blue2BBoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorBlack,
    boxColorBPP = colorBlue,
    leftIconBPP = boxLeftIcon2,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

green2BBoxPP :: BoxPP -- Urgent workspace
green2BBoxPP = BoxPP
    {
    bgColorBPP = colorBlack,
    fgColorBPP = colorBlack,
    boxColorBPP = colorGreen,
    leftIconBPP = boxLeftIcon2,
    rightIconBPP = boxRightIcon,
    boxHeightBPP = boxHeight
    }

-- Dzen logger clickable areas
layoutCA :: CA
layoutCA = CA
    {
    leftClickCA = "xdotool key super+space"
    middleClickCA = "xdotool key super+v"
    rightClickCA = "xdotool key super+shift+space"
    wheelUpCA = "xdotool key super+f"
    wheelDownCA = "xdotool key super+control+f"
    }

workspaceCA :: CA
workspaceCA = CA
    {
    leftClickCA = "xdotool key super+1"
    middleClickCA = "xdotool key super+g"
    rightClickCA = "xdotool key super+0"
    wheelUpCA = "xdotool key ctrl+alt+Right"
    wheelDownCA = "xdotool key ctrl+alt+Left"
    }

focusCA :: CA
focusCA = CA
    {
    leftClickCA = "xdotool key super+m"
    middleClickCA = "xdotool key super+c"
    rightClickCA = "xdotool key super+shift+m"
    wheelUpCA = "xdotool key super+shift+j"
    wheelDownCA = "xdotool key super+shift+k"
    }

-- Workspaces index (don't change)
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show $ [1 .. 9] ++ [0]

-- Fonts, icons, etc
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
