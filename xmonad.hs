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

boxLeftIcon = "~/.xmonad/xbm_icons/boxleft.xbm"
boxLeftIcon2 = "~/.xmonad/xbm_icons/boxleft2.xbm"
boxRightIcon = "~/.xmonad/xbm_icons/boxright.xbm"

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
calendarCA :: CA
calendarCA = CA
    {
    leftClickCA = "~/.xmonad/dzencal.sh"
    middleClickCA = "~/.xmonad/dzencal.sh"
    rightClickCA = "~/.xmonad/dzencal.sh"
    wheelUpCA = "~/.xmonad/dzencal.sh"
    wheelDownCA = "~/.xmonad/dzencal.sh"
    }

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

-- Workspace names
workSpaceNames :: [WorkspaceId]
workSpaceNames =
    [
    "Terminal",
    "Web",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    "Other",
    ]

-- Layout names (must be 1 word)
myTileName = "Tiled"
myMirrName = "MirrorTiled"
myMosAName = "Mosaic"
myOneBName = "OneBig"
myCst1Name = "Default"
myCst2Name = "MasterTab"
myCst3Name = "Web"
myChatName = "Chat"
myFTabName = "FullTab"
myFloaName = "Float"

-- Startup hook config
myStartupHook =
    (setDefaultCursor xC_left_ptr) <+>
    (spawn "feh --bg-scale ~/Pictures/background.jpg") <+>
    (startTimer 1 >>= XS.put . TID)

-- Event hook config
-- Wrapper for the timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
    initialValue = TID 0

-- Handle event hook
myHandleEventHook =
    fullscreenEventHook <+>
    docksEventHook <+>
    clockEventHook <+>
    handleTimerEvent <+>
    notFocusFloat where
        clockEventHook e = do
            (TID t) <- XS.get
            handleTimer t e $ do
                startTimer 1 >>= XS.put . TID
                ask >>= logHook . config
                return Nothing
            return $ All True
        notFocusFloat = followOnlyIf (fmap not isFloat) where
            isFloat = fmap (isSuffixOf myFloatName) $ gets (description . W.layout . W.workspace . W.current . windowset)

-- Layout config
-- Tabbed transformer (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
    transform TABBED x k = k myFTabU (\_ -> X)

-- Floated transformer (W+ctrl+f)
data FLOATED = FLOATED deriving (Read, Show, Eq, Typeable)
instance Transformer FLOATED Window where
    transform FLOATED x k = k myFloaU (\_ -> X)

-- Unique layouts
myFTabU = smartBorders $ named ("Unique " ++ myFTabName) $ tabbedAlways shrinkText myTitleTheme
myFloaU = named ("Unique " ++ myFloaName) $ mouseResize $ noFrillsDeco shrinkText myTitleTheme simplestFloat

-- Layout hook
myLayoutHook =
    gaps [(U, panelHeight), (D, panelHeight)] $
    configurableNavigation noNavigationBorders $
    minimize $
    maximize $
    mkToggle (single TABBED) $
    mkToggle (single FLOATED) $
    mkToggle (single MIRROR) $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    onWorkspace (myWorkspaces !! 1) codeLayouts $
    onWorkspace (myWorkspaces !! 2) webLayouts $
    allLayouts where
        webLayouts = (myToggleL myCst3 myCst3Name) ||| (myToggleL myCst1 myCst1Name)
        codeLayouts = (myToggleL myCst2 myCst2Name) ||| (myToggleL myOneB myOneBName) ||| (myToggleL myTile myTileName)
        allLayouts =
            (myToggleL myCst1 myCst1Name) |||
            (myToggleL myCst2 myCst2Name) |||
            (myToggleL myTile myTileName) |||
            (myToggleL myOneB myOneBName) |||
            (myToggleL myMirr myMirrName) |||
            (myToggleL myMosA myMosAName) |||
            (myToggleL myCst3 myCst3Name)
        -- layouts
        myTile = ResizableTall 1 0.03 0.5 []
        myMirr = Mirror myTile
        myMosA = MosaicAlt M.empty
        myOneB = OneBig 0.75 0.65
        myCst1 = (layoutN 2 (relBox 0 0 1 0.6) (Just $ relBox 0 0 1 1) $ myTile) $ (layoutAll (relBox 0 0.6 1 1) $ myTabb)
        myCst2 = (layoutN 1 (relBox 0 0 0.4 1) (Just $ relBox 0 0 1 1) $ myTile) $ (layoutAll (relBox 0.4 0 1 1) $ myTabb)
        myCst3 = (layoutN 1 (relBox 0 0 1 0.7) (Just $ relBox 0 0 1 1) $ myTabb) $ (layoutAll (relBox 0 0.7 1 1) $ myTabb)
        myTabb = tabbed shrinkText myTitleTheme
        -- custom dragging visualizer toggle
        myToggleL 1 n = smartBorders $ toggleLayouts (named ("Switcher " ++ n) $ switcher 1) (named ("Normal " ++ n) 1) where
            switcher 1 = windowSwitcherDecoration shrinkText myTitleTheme $ draggingVisualizer 1

-- Manage hook config
myManageHook :: ManageHook
myManageHook =
    manageDocks <+>
    (scratchpadManageHook $ W.RationalRect 0 0 1 (3 / 4)) <+>
    dynamicMasterHook <+>
    manageWindows

-- Manage windows
manageWindows :: ManageHook
manageWindows = composeAll . concat $
    [
    [ resource =? r --> doIgnore | r <- myIgnores ],
    [ className =? c --> doShift (myWorkspaces !! 1) | c <- myWebS ],
    [ className =? c --> doShift (myWorkspaces !! 2) | c <- myCodeS ],
    [ className =? c --> doShift (myWorkspaces !! 3) | c <- myGfxS ],
    [ className =? c --> doShift (myWorkspaces !! 4) | c <- myChatS ],
    [ className =? c --> doShift (myWorkspaces !! 9) | c <- myAlt3S ],
    [ className =? c --> doCenterFloat | c <- myFloatCC ],
    [ name =? n --> doCenterFloat | n <- myFloatCN ],
    [ name =? n --> doSideFloat NW | n <- myFloatSN ],
    [ className =? c --> doF W.focusDown | n <- myFocusDC ],
    [ currentWs =? (myWorkspaces !! 1) --> keepMaster "Chrome" ],
    [ isFullscreen --> doFullFloat ]
    ] where
        name = stringProperty "WM_NAME"
        myIgnores = ["desktop", "desktop_window"]
        myWebS = ["Chrome"]
        myCodeS = [""]
        myGfxS = ["Gimp", "gimp", "GIMP"]
        myAlt3S = ["Transmission-gtk"]
        myFloatCC = ["File-roller", "XClock"]
        myFloatCN = ["Choose a file", "Open Image", "File Operation Progress",
            "Preferences", "Search Engines", "Rename File", "Copying files",
            "Moving files", "File properties", "Replace", "Quit GIMP",
            "Change Foreground Color", "Change Background Color", ""]
        myFloatSN = ["Event Tester"]
        myFocusDC = ["Event Tester", "Notify-osd"]
        keepMaster c = assertSlave <+> assertMaster where
            assertSlave = fmap (/ = c) className --> doF W.swapDown
            assertMaster = className =? c --> doF W.swapMaster

-- Dzen status bar config
-- Urgency hook
myUrgencyHook :: LayoutClass 1 Window => XConfig 1 -> XConfig 1
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
    duration = 2000000,
    args =
        [
        "-x", "0",
        "-y", "0",
        "-h", show panelHeight,
        "-w", show topPanelSepPos,
        "-fn", dzenFont,
        "-bg", colorBlack,
        "-fg", colorGreen
        ]
    }

-- Dzen top left bar flags
dzenTopLeftFlags :: Res -> DF
dzenTopLeftFlags _ = DF
    {
    xPosDF = 0,
    yPosDF = 0,
    widthDF = topPanelSepPos,
    heightDF = panelHeight,
    alignmentDF = "1",
    fgColorDF = colorWhiteAlt,
    bgColorDF = colorBlack,
    fontDF = dzenFont,
    eventDF = "onstart=lower",
    extrasDF = "-p"
    }

-- Top left bar loghook
myTopLeftLogHook :: Handle -> X ()
myTopLeftLogHook h = dynamicLogWithPP defaultPP
    {
    ppOutput = hPutStrLn h,
    ppOrder = \(_:_:_:x) -> x,
    ppSep = " ",
    ppExtras = [ myLayoutL, myWorkspaceL, myFocusL ]
    }

-- Dzen top right bar flags
dzenTopRightFlags :: Res -> DF
dzenTopRightFlags r = DF
    {
    xPosDF = topPanelSepPos,
    yPosDF = 0,
    widthDF = (xRes r) - topPanelSepPos,
    heightDF = panelHeight,
    alignmentDF = "r",
    fgColorDF = colorWhiteAlt,
    bgColorDF = colorBlack,
    fontDF = dzenFont,
    eventDF = "onstart=lower",
    extrasDF = "-p"
    }

-- Top right bar log hook
myTopRightLogHook :: Handle -> X ()
myTopRightLogHook h = dynamicLogWithPP defaultPP
    {
    ppOutput = hPutStrLn h,
    ppOrder = \(_:_:_:x) -> x,
    ppSep = " ",
    ppExtras = [ myUptimeL, myDateL ]
    }

-- Dzen bottom left bar flags
dzenBotLeftFlags :: Res -> DF
dzenBotLeftFlags r = DF
    {
    xPosDF = 0,
    yPosDF = (yRes r) - panelHeight,
    widthDF = botPanelSepPos,
    heightDF = panelHeight,
    alignmentDF = "1",
    fgColorDF = colorWhiteAlt,
    bgColorDF = colorBlack,
    fontDF = dzenFont,
    eventDF = "onstart=lower",
    extrasDF = "-p"
    }

-- Bottom left bar log hook
myBotLeftLogHook :: Handle -> X ()
myBotLeftLogHook h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ defaultPP
    {
    ppOutput = hPutStrLn h,
    ppOrder = \(ws:_:_:x) -> [ ws ] ++ x
    ppSep = " ",
    ppWsSep = "",
    ppCurrent = dzenBoxStyle blue2BBoxPP
    ppUrgent = dzenBoxStyle green2BBoxPP . dzenClickWorkspace
    ppVisible = dzenBoxStyle blackBoxPP . dzenClickWorkspace
    ppHiddenNoWindows = dzenBoxStyle blackBoxPP . dzenClickWorkspace
    ppHidden = dzenBoxStyle whiteBoxPP . dzenClickWorkspace
    ppExtras = [ myResL, myBrightL ]
    } where
        dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()" where
            wsIdxToString Nothing = "1"
            wsIdxToString (Just n) = show $ mod (n + 1) $ length myWorkspaces
            index = wsIdxToString (elemIndex ws myWorkspaces)
            xdo key = "xdotool key super+" ++ key

-- Dzen bottom right bar flags
dzenBotRightFlags :: Res -> DF
dzenBotRightFlags r = DF
    {
    xPosDF = botPanelSepPos,
    yPosDF = (yRes r) - panelHeight,
    widthDF = (xRes r) - botPanelSepPos,
    heightDF = panelHeight,
    alignmentDF = "r",
    fgColorDF = colorBlue,
    bgColorDF = colorBlack,
    fontDF = dzenFont,
    eventDF = "onstart=lower",
    extrasDF = "-p"
    }

-- Bottom right bar log hook
myBotRightLogHook :: Handle -> X ()
myBotRightLogHook h = dynamicLogWithPP defaultPP
    {
    ppOutput = hPutStrLn h,
    ppOrder = \(_:_:_:x) -> x,
    ppSep = " ",
    ppExtras = [ myCpuL, myMemL, myTempL, myWifiL, myBatL ]
    }

-- Loggers config
-- Bottom right loggers
myBatL =
    (dzenBoxStyleL gray2BoxPP $ labelL "BATTERY") ++!
    (dzenBoxStyleL blueBoxPP $ batPercent 30 colorRed) ++!
    (dzenBoxStyleL whiteBoxPP batStatus)

myWifiL =
    (dzenBoxStyleL gray2BoxPP $ labelL "WIFI") ++!
    (dzenBoxStyleL blueBoxPP wifiSignal)

myTempL =
    (dzenBoxStyleL gray2BoxPP $ labelL "TEMP") ++!
    (dzenBoxStyleL blueBoxPP $ cpuTemp 2 colorRed)

myMemL =
    (dzenBoxStyleL gray2BoxPP $ labelL "MEM") ++!
    (dzenBoxStyleL blueBoxPP $ memUsage [ percMemUsage, totMBMemUsage ])

-- Bottom left loggers
myResL =
    (dzenBoxStyleL blue2BoxPP $ labelL "RES") ++!
    (dzenBoxStyleL whiteBoxPP $ screenRes ":0" 0)

myBrightL =
    (dzenBoxStyleL blue2BoxPP $ labelL "BRIGHT") ++!
    (dzenBoxStyleL whiteBoxPP $ brightPerc 10)

-- Top right loggers
myDateL =
    (dzenBoxStyleL white2BBoxPP $ date "%A") ++!
    (dzenBoxStyleL whiteBoxPP $ date $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg()") ++!
    (dzenBoxStyleL whiteBoxPP $ date $ "%H^fg(" ++ colorGray ++ "):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorBlue ++ ")%S^fg()") ++!
    (dzenClickStyleL calendarCA $ dzenBoxStyleL blueBoxPP $ labelL "CALENDAR")

myUptimeL =
    (dzenBoxStyleL blue2BoxPP $ labelL "UPTIME") ++!
    (dzenBoxStyleL whiteBoxPP uptime)

-- Top left loggers
myFocusL =
    (dzenClickStyleL focusCA $ dzenBoxStyleL white2BBoxPP $ labelL "FOCUS") ++!
    (dzenBoxStyleL whiteBoxPP $ shortenL 100 logTitle)

myLayoutL =
    (dzenClickStyleL layoutCA $ dzenBoxStyleL blue2BoxPP $ labelL "LAYOUT") ++!
    (dzenBoxStyleL whiteBoxPP $ onLogger (layoutText . removeWord . removeWord) logLayout) where
        removeWord xs = tail $ dropWhile (/= ' ') xs
        layoutText xs
            | isPrefixOf "Mirror" xs = layoutText $ removeWord xs ++ "^fg(" ++ colorBlue ++ ")M^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
            | isPrefixOf "ReflectY" xs = layoutText $ removeWord xs ++ "^fg(" ++ colorBlue ++ ")Y^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
            | isPrefixOf "ReflectX" xs = layoutText $ removeWord xs ++ "^fg(" ++ colorBlue ++ ")X^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
            | isPrefixOf "Switcher" xs = layoutText $ removeWord xs ++ "^fg(" ++ colorRed ++ ")S^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
            | isPrefixOf "Normal" xs = layoutText $ removeWord xs ++ "^fg(" ++ colorGreen ++ ")N^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
            | isPrefixOf "Unique" xs = layoutText $ removeWord xs ++ "^fg(" ++ colorGreen ++ ")U^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
            | otherwise = concat $ reverse $ words xs

myWorkspaceL =
    (dzenClickStyleL workspaceCA $ dzenBoxStyleL blue2BoxPP $ labelL "WORKSPACE") ++!
    (dzenBoxStyleL whiteBoxPP $ onLogger namedWorkspaces logCurrent) where
        namedWorkspaces w
            | (elem w $ map show [ 0 .. 9 ]) = "^fg(" ++ colorGreen ++ ")" ++ w ++ "^fg(" ++ colorGray ++ ")|^fg()" ++ workSpaceNames !! (mod ((read w::Int) - 1) 10)
            | otherwise = "^fg(" ++ colorRed ++ ")x^fg(" ++ colorGray ++ ")|^fg()" ++ w

-- Bindings config
-- Key bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modMask }) = M.fromList $
    -- xmonad bindings
    [
    ((modMask .|. shiftMask, xK_q), killAndExit),
    ((modMask, xK_q), killAndRestart),
    ((mod1Mask, xK_F2), shellPrompt myXPConfig),
    ((modMask, xK_F2), xmonadPrompt myXPConfig),
    --((mod1Mask, xK_F3), manPrompt myXPConfig),
    ((modMask, xK_g), goToSelected $ myGSConfig myColorizer),
    ((modMask, xK_masculine), scratchPad),
    ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),

    -- window management bindings
    ((modMask, xK_c), kill),
    ((modMask, xK_r), refresh),
    ((modMask, xK_j), windows W.focusDown), -- focus next
    ((modMask, xK_k), windows W.focusUp), -- focus prev
    ((modMask, xK_m), windows W.focusMaster), -- focus master
    ((modMask .|. shiftMask, xK_j), windows W.swapDown), -- swap next
    ((modMask .|. shiftMask, xK_k), windows W.swapUp), -- swap prev
    ((modMask .|. shiftMask, xK_m), windows W.swapMaster), -- swap master
    ((modMask, xK_h), sendMessage Shrink), -- shrink master
    ((modMask, xK_l), sendMessage Expand), -- expand master
    ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink), -- mirror shrink master
    ((modMask .|. shiftMask, xK_l), sendMessage MirrorExpand), -- mirror expand master
    ((modMask, xK_t), withFocused $ windows . W.sink), -- tile window
    ((modMask .|. shiftMask, xK_t), rectFloatFocused), -- float window
    ((modMask, xK_comma), sendMessage (IncMasterN 1)), -- increment master area
    ((modMask, xK_period), sendMessage (IncMasterN (-1))), -- decrement master area

    -- layout management bindings
    ((modMask, xK_space), sendMessage NextLayout),
    ((modMask, xK_v), sendMessage ToggleLayout),
    ((modMask .|. shiftMask, xK_space), flashText myTextConfig 1 " set to Default Layout " >> (setLayout $ XMonad.layoutHook conf)),
    ((modMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED),
    ((modMask .|. controlMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle FLOATED),
    ((modMask .|. shiftMask, xK_x), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX),
    ((modMask .|. shiftMask, xK_y), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY),
    ((modMask .|. shiftMask, xK_z), sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
    ]

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig { XMonad.modMask = modMask }) = M.fromList $
    [
    ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
    ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)),
    ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),
    ((modMask, button4), (\_ -> prevWS)),
    ((modMask, button5), (\_ -> nextWS)),
    ((modMask .|. shiftMask, button4), (\_ -> shiftToPrev)),
    ((modMask .|. shiftMask, button5), (\_ -> shiftToNext))
    ]

-- Dzen utils
-- Dzen flags
data DF = DF
    {
    xPosDF :: Int,
    yPosDF :: Int,
    widthDF :: Int,
    heightDF :: Int,
    alignmentDF :: String,
    fgColorDF :: String,
    bgColorDF :: String,
    fontDF :: String,
    eventDF :: String,
    extrasDF :: String
    }

-- Dzen box pretty config
data BoxPP = BoxPP
    {
    bgColorBPP :: String,
    fgColorBPP :: String,
    fgColorBPP :: String,
    boxColorBPP :: String,
    leftIconBPP :: String,
    rightIconBPP :: String,
    boxHeightBPP :: String
    }

-- Dzen clickable area config
data CA = CA
    {
    leftClickCA :: String,
    middleClickCA :: String,
    rightClickCA :: String,
    wheelUpCA :: String,
    wheeldownCA :: String
    }

-- Create a dzen string with its flags
dzenBoxStyle :: BoxPP -> String -> String
dzenBoxStyle bpp t =
        "^fg(" ++ (boxColorBPP bpp) ++
        ")^i(" ++ (leftIconBPP bpp) ++
        ")^fg(" ++ (bgColorBPP bpp) ++
        ")^r(1024x" ++ (show $ boxHeightBPP bpp) ++
        ")^p(-1024)^fg()^ib(0)"

-- Uses dzen format to make dzen text clickable
dzenClickStyle :: BoxPP -> String -> String
dzenClickStyle ca t =
    "^ca(1, " ++ leftClickCA ca ++
    "^ca((2, " ++ middleClickCA ca ++
    "^ca(3, " ++ rightClickCA ca ++
    "^ca(4, " ++ wheelUpCA ca ++
    "^ca(5, " ++ wheelDownCA ca ++
    ")" ++ t ++
    "^ca()^ca()^ca()^ca()^ca()"

-- Launch dzen through the system shell and return a handle to its standard input
dzenSpawnPipe df = spawnPipe $ "dzen2" ++ dzenFlagsToStr df

-- Logger version of dzenBoxStyle
dzenBoxStyleL :: BoxPP -> Logger -> Logger
dzenBoxStyleL bpp l = (fmap . fmap) (dzenBoxStyle bpp) l

-- Logger version of dzenClickStyle
dzenClickStyleL :: CA -> Logger -> Logger
dzenClickStyleL ca l = (fmap . fmap) (dzenClickStyle ca) l

-- Loggers
-- Concat 2 loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = (liftA2 . liftA2) (++) l1 l2

-- Label
labelL :: String -> Logger
labelL = return . return
