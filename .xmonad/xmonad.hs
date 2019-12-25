-- Imports --------------------------------------------------------------------
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import Data.List
import qualified XMonad.StackSet as W

import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed

import XMonad.Actions.CycleWS

import System.IO

-- General --------------------------------------------------------------------

-- Terminal
myTerminal              = "urxvt"

-- Modifier key
myModMask               = mod4Mask

-- Colors
myForegroundColor       = "#ffffff"
myBackgroundColor       = "#000000"
myAccentColor           = "#fd1369"

-- Borders
myBorderWidth           = 1
myNormalBorderColor     = "#000000"
myFocusedBorderColor    = "#fd1369"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse     = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses      = False

-- Workspaces -----------------------------------------------------------------

myWorkspaces = [" 一 ", " 二 ", " 三 ", " 四 ", " 五 ", " 六 ", " 七 ", " 八 ", " 九 ", " 十 "]

-- Key bindings ---------------------------------------------------------------
-- Add, modify or remove key bindings here.

myKeys =    [ ((modm, xK_Left), prevWS)
            , ((modm, xK_Right), nextWS)
            , ((modm .|. controlMask              , xK_s    ), sendMessage  Arrange          )
            , ((modm .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange        )
            , ((modm .|. controlMask              , xK_Left ), sendMessage (MoveLeft      10))
            , ((modm .|. controlMask              , xK_Right), sendMessage (MoveRight     10))
            , ((modm .|. controlMask              , xK_Down ), sendMessage (MoveDown      10))
            , ((modm .|. controlMask              , xK_Up   ), sendMessage (MoveUp        10))
            , ((modm                 .|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  10))
            , ((modm                 .|. shiftMask, xK_Right), sendMessage (IncreaseRight 10))
            , ((modm                 .|. shiftMask, xK_Down ), sendMessage (IncreaseDown  10))
            , ((modm                 .|. shiftMask, xK_Up   ), sendMessage (IncreaseUp    10))
            , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  10))
            , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 10))
            , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  10))
            , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    10))
            , ((modm, xK_KP_Add), sequence_ [ sendMessage (IncreaseLeft 10)
                        , sendMessage (IncreaseRight 10)
                        , sendMessage (IncreaseUp 10)
                        , sendMessage (IncreaseDown 10)
                        ])
            , ((modm, xK_KP_Subtract), sequence_ [ sendMessage (DecreaseLeft 10)
                             , sendMessage (DecreaseRight 10)
                             , sendMessage (DecreaseUp 10)
                             , sendMessage (DecreaseDown 10)
                             ])
        -- programs
        -- rofi, power, lock
            , ((modm, xK_p), spawn $ "rofi -show drun -theme themes/appsmenu.rasi")
            , ((modm .|. controlMask, xK_p), spawn $ "rofi -show combi -theme themes/appsmenu.rasi")
            , ((modm, xK_o), spawn $ "xtrlock")
            , ((modm .|. controlMask, xK_o), spawn $ "sh ~/.config/rofi/scripts/powermenu.sh")
            ] where modm = myModMask

-- Layouts --------------------------------------------------------------------

myLayout = gaps [(U, 35), (R, 5), (L, 5), (D, 5)] $ smartSpacing 5 $ (tiled ||| Circle ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
-- By default, do nothing.
myStartupHook :: X()
myStartupHook = do
    setWMName "xmonad"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.

main = do
    bar <- spawnPipe "xmobar ~/.xmobarrc"
    xmonad $ def {
          -- simple stuff
            terminal           = myTerminal,
            focusFollowsMouse  = myFocusFollowsMouse,
            clickJustFocuses   = myClickJustFocuses,
            borderWidth        = myBorderWidth,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,

          -- hooks, layouts
            layoutHook         = myLayout,
            manageHook         = myManageHook,
            handleEventHook    = myEventHook,
            logHook            = myLogHook bar,
            startupHook        = myStartupHook
        } `additionalKeys` myKeys
