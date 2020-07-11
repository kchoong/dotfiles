-- Imports --------------------------------------------------------------------
import XMonad
import XMonad.Core

import Data.List
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Exit
import System.IO

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Types (Rectangle)

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Combo
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Workspace

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- General --------------------------------------------------------------------

-- Terminal
myTerminal              = "urxvt"

-- Bar
myBar                   = "xmobar ~/.xmonad/.xmobarrc"

-- Screen locker
myLocker                = "xtrlock"

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

-- Screens --------------------------------------------------------------------

xdisplays :: X [Rectangle]
xdisplays = withDisplay $ io . getScreenInfo

-- Mouse bindings -------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- Key bindings ---------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Custom key bindings ----------------------------------------------------
    -- Terminal
    [ ((modMask .|. shiftMask,  xK_Return), spawn $ myTerminal)
    -- Lock
    , ((modMask, xK_o), spawn $ myLocker)
    -- Rofi drun menu
    , ((modMask, xK_p), spawn $ "rofi -show drun -theme themes/appsmenu.rasi")
    -- Rofi combi menu
    , ((modMask .|. shiftMask, xK_p), spawn $ "rofi -show combi -theme themes/appsmenu.rasi")
    -- Rofi power menu
    , ((modMask .|. shiftMask, xK_o), spawn $ "sh ~/.config/rofi/scripts/powermenu.sh")
    ]

    ++

    -- Fn keys ----------------------------------------------------------------
    -- Volume down
    [ ((0, xF86XK_AudioLowerVolume), spawn "pulsemixer --change-volume -2")
    -- Volume up
    , ((0, xF86XK_AudioRaiseVolume), spawn "pulsemixer --change-volume +2")
    -- Volume mute
    , ((0, xF86XK_AudioMute), spawn "pulsemixer --id sink-0 --toggle-mute")
    -- Screen brightness down
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    -- Screen brightness up
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    ]

    ++

    -- "Standard" xmonad key bindings -----------------------------------------
    -- Close focused window.
    [ ((modMask .|. shiftMask, xK_c), kill)

    -- Cycle through the available layout algorithms.
    , ((modMask, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default.
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size.
    , ((modMask, xK_n), refresh)

    -- Move focus to the next window.
    , ((modMask, xK_Tab), windows W.focusDown)

    -- Move focus to the next window.
    , ((modMask, xK_j), windows W.focusDown)

    -- Move focus to the previous window.
    , ((modMask, xK_k), windows W.focusUp)

    -- Move focus to the master window.
    , ((modMask, xK_m), windows W.focusMaster)

    -- Swap the focused window and the master window.
    , ((modMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window.
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)

    -- Swap the focused window with the previous window.
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)

    -- Shrink the master area.
    , ((modMask, xK_h), sendMessage Shrink)

    -- Expand the master area.
    , ((modMask, xK_l), sendMessage Expand)

    -- Push window back into tiling.
    , ((modMask, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area.
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area.
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap.
    -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

    -- Quit xmonad.
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad.
    , ((modMask, xK_q), restart "xmonad" True)
    ]

    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++

    -- Switch workspaces and screens
    [ ((modMask, xK_Right), nextWS)
    , ((modMask, xK_Left), prevWS)
    , ((modMask, xK_Down), nextScreen)
    , ((modMask, xK_Up), prevScreen)
    , ((modMask .|. shiftMask, xK_Right), shiftToNext)
    , ((modMask .|. shiftMask, xK_Left), shiftToPrev)
    , ((modMask .|. shiftMask, xK_Down), shiftNextScreen)
    , ((modMask .|. shiftMask, xK_Up), shiftPrevScreen)
    , ((modMask, xK_z), toggleWS)]

    ++

    -- Screenlayouts
    [ ((modMask .|. controlMask, xK_1),  spawn $ "sh ~/.screenlayout/primary.sh")
    , ((modMask .|. controlMask, xK_2),  spawn $ "sh ~/.screenlayout/mod-extended.sh")
    , ((modMask .|. controlMask, xK_3),  spawn $ "sh ~/.screenlayout/duplicated.sh")
    ]

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
    setWMName "LG3D"

-- Main -----------------------------------------------------------------------

main :: IO ()
main = do
    bar <- spawnPipe $ myBar
    xmonad $ defaults {
        logHook = myLogHook bar
    }

defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    -- logHook            = myLogHook,
    startupHook        = myStartupHook
}
