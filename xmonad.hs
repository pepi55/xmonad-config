-- Imports
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Layout.Spacing
import Graphics.X11.ExtraTypes.XF86

-- (hPutStrLn)
-- import System.Exit(ExitCode(ExitSuccess), exitWith)
-- import XMonad.Config.Gnome(gnomeConfig)
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Config.Desktop(desktopLayoutModifiers)
-- import XMonad.Layout.NoBorders(smartBorders)
-- import XMonad.Hooks.ManageHelpers(isFullscreen, isDialog, doFullFloat)
-- import XMonad.Hooks.UrgencyHook(NoUrgencyHook(NoUrgencyHook), withUrgencyHook)
-- import XMonad.Layout.ComboP(Property(Role))
-- import XMonad.Layout.PerWorkspace(onWorkspace)
-- import Control.Monad(liftM2)
-- import XMonad.Layout.IM(withIM)
-- import XMonad.Layout.Reflect(reflectHoriz)
-- import qualified XMonad.StackSet as W (sink, shift, greedyView)

myLayout = tiled ||| Mirror tiled ||| Full
	where
		tiled = spacing 5 $ Tall nmaster delta ratio
		nmaster = 1
		ratio = 1/2
		delta = 3/100

myManageHook = composeAll
	[ className =? "Gimp" --> doFloat ]

main = do

	xmproc <- spawnPipe "xmobar"	

	xmonad $ defaultConfig
		{ modMask							= mod4Mask
		,	manageHook					=	manageDocks <+> myManageHook -- manageHook defaultConfig
		,	layoutHook					=	avoidStruts $ myLayout -- layoutHook defaultConfig
		,	logHook							=	dynamicLogWithPP xmobarPP
																{ ppOutput	= hPutStrLn xmproc
																,	ppTitle		= xmobarColor barCol "" . shorten 100
																,	ppLayout	=	const "" -- Tall layout info bar enz
																}
		,	borderWidth					= 2
		,	terminal						= "urxvt"
		,	normalBorderColor		= "#CCCCCC"
		,	focusedBorderColor	= "#026CB8"
		} `additionalKeys`
		[ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
		,	((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
		, ((0, xK_Print), spawn "scrot")
		,	((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
		,	((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
		,	((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
		]

barCol = "#00A0E0"
