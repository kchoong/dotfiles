-- Imports --------------------------------------------------------------------
import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer

import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import Data.List
import qualified Data.Map as M
import System.Exit
import System.IO
import Graphics.X11.ExtraTypes.XF86

-- Main -----------------------------------------------------------------------

main = do
    bar <- spawnPipe "xmobar -x" ++ show sid
    xmonad $ defaultConfig {
        -- general
        terminal            = myTerminal,
        focusFollowsMouse   = myFocusFollowsMouse,
        clickJustFocuses    = myClickJustFocuses,
        borderWidth         = myBorderWidth,
        modMask             = myModMask,
        workspaces          = myWorkspaces,
        normalBorderColor   = myNormalBorderColor,
        focusedBorderColor  = myFocusedBorderColor,

        -- mouse and key bindings
        mouseBindings       = myMouseBindings,
        keys                = myKeys,

        -- hooks, layouts
        layoutHook          = myLayout,
        manageHook          = myManageHook,
        handleEventHook     = myEventHook,
        logHook             = myLogHook bar,
        startupHook         = myStartupHook
    }

-- General --------------------------------------------------------------------

-- Terminal
myTerminal              = "urxvt"

-- Modifier key
myModMask               = mod4Mask

-- Colors
myForegroundColor       = "#dfdfdf"
myBackgroundColor       = "#000000"
myAccentColor           = "#fd1369"

-- Borders
myBorderWidth           = 1
myNormalBorderColor     = "#000000"
myFocusedBorderColor    = "#dfdfdf"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse     = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses      = False

-- Workspaces -----------------------------------------------------------------

myWorkspaces = [" 一 ", " 二 ", " 三 ", " 四 ", " 五 ", " 六 ", " 七 ", " 八 ", " 九 ", " 十 "]

-- Mouse bindings -------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Key bindings ---------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)

    -- Grid Select
    , ((modm, xK_g), goToSelected defaultGSConfig)

     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)

    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Switch workspaces and screens
    --, ((modm, xK_Right), moveTo next)
    --, ((modm, xK_Left), moveTo prev)
    --, ((modm .|. shiftMask, xK_Right), shiftTo next)
    --, ((modm .|. shiftMask, xK_Left), shiftTo prev)
    --, ((modm, xK_Down), nextScreen)
    --, ((modm, xK_Up), prevScreen)
    --, ((modm .|. shiftMask, xK_Down), shiftNextScreen)
    --, ((modm .|. shiftMask, xK_Up), shiftPrevScreen)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]

    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++

    -- rofi, power, lock
    [ ((modm, xK_p), spawn $ "rofi -show drun -theme themes/appsmenu.rasi")
    , ((modm .|. shiftMask, xK_p), spawn $ "rofi -show combi -theme themes/appsmenu.rasi")
    , ((modm, xK_o), spawn $ "xtrlock")
    , ((modm .|. shiftMask, xK_o), spawn $ "sh ~/.config/rofi/scripts/powermenu.sh")
    ]

    ++

    -- fn keys
    [ ((0, xF86XK_AudioLowerVolume), spawn "pulseaudio-ctl down 10")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pulseaudio-ctl up 10")
    , ((0, xF86XK_AudioMute), spawn "pulseaudio-ctl mute")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    ] where modm = myModMask

-- Layouts --------------------------------------------------------------------

-- Basic tiled layout
myBasicLayout = Tall nmaster delta ratio where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 3/5
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- Tiled layout
myTiledLayout = named "Tiled" $ avoidStruts $ smartSpacing 2 $ gaps [(U, 35), (R, 5), (L, 5), (D, 5)] $ myBasicLayout

-- Grid Layout
myGridLayout = named "Grid" $ avoidStruts $ smartSpacing 2 $ gaps [(U, 35), (R, 5), (L, 5), (D, 5)] $ Grid

-- Fullscreen layout
myFullscreenLayout = named "Fullscreen" $ noBorders Full

-- Layout options
myLayout = windowNavigation $ (myTiledLayout ||| myGridLayout ||| myFullscreenLayout)

-- Window rules ---------------------------------------------------------------

myManageHook = composeAll
    []
    <+> (isFullscreen --> doFullFloat)
    <+> manageDocks
    <+> manageHook def

-- Event handling hook --------------------------------------------------------

myEventHook = mempty

-- Status bars and logging hook -----------------------------------------------

myLogHook h = dynamicLogWithPP $ defaultPP

    -- display current workspace as darkgrey on light grey (opposite of
    -- default colors)
    { ppCurrent         = xmobarColor (myForegroundColor) (myAccentColor) . pad

    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = xmobarColor (myForegroundColor) (myBackgroundColor) . pad

    -- display other workspaces with no windows as a normal grey
    -- , ppHiddenNoWindows = xmobarColor (myForegroundColor) (myBackgroundColor) . pad

    -- display the current layout as a brighter grey
    , ppLayout          = xmobarColor (myForegroundColor) (myAccentColor) . pad

    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = xmobarColor (myForegroundColor) (myAccentColor) . pad

    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100

    --
    , ppOrder           = \(ws:l:t:_) -> [ws,l]

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "    ::    "

    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }

-- Startup hook ---------------------------------------------------------------

myStartupHook :: X()
myStartupHook = do
    setWMName "xmonad"

-- Help -----------------------------------------------------------------------

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch terminal",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
