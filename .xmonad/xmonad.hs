import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run

import XMonad.Layout.FixedColumn
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Prompt
import qualified XMonad.Layout.BoringWindows as B

import System.Exit
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

import System.IO (hClose)

import qualified Codec.Binary.UTF8.String as UTF8

-- main :: IO()
main = do
  xmonad
    $ myConfig
    `additionalKeys`
                [ ((mod4Mask, space), spawn "rofi -show drun -theme themes/appsmenu.rasi")
                ]

-- CONFIG

myConfig = def
  { terminal           = myTerminal,
  -- focusFollowsMouse  = myFocusFollowsMouse,
  borderWidth        = myBorderWidth,
  modMask            = myModMask,
  -- numlockMask deprecated in 0.9.1
  -- numlockMask        = myNumlockMask,
  workspaces         = myWorkspaces,
  -- normalBorderColor  = myNormalBorderColor,
  -- focusedBorderColor = myFocusedBorderColor,

  -- key bindings
  keys               = myProgramKeys,
  -- mouseBindings      = myMouseBindings,

  -- hooks, layouts
  -- layoutHook         = myLayout,
  -- manageHook         = myManageHook,
  -- handleEventHook    = myEventHook,
  -- logHook            = myLogHook,
  startupHook        = myStartupHook
  }

-- General config
myTerminal     = "urxvt"
myModMask      = mod4Mask
myBorderWidth  = 1
myBrowser      = "chromium"
mySpacing :: Int
mySpacing      = 5
myLargeSpacing :: Int
myLargeSpacing = 30
noSpacing :: Int
noSpacing      = 0
prompt         = 20

-- Colours
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

-- Font
myFont = "xft:SF Pro Display:" ++ "fontformat=truetype:size=10:antialias=true"

-- LAYOUT

-- THEMES
-- Prompt themes


-- WORKSPACES
wsGEN = "\xf269"
wsWRK = "\xf02d"
wsSYS = "\xf300"
wsMED = "\xf001"
wsTMP = "\xf2db"
wsGAM = "\xf11b"

myWorkspaces :: [String]
myWorkspaces = [wsGEN, wsWRK, wsSYS, wsMED, wsTMP, wsGAM, "7", "8", "9"]

-- KEYBINDINGS
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=adobe courier"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
  myProgramKeys ++ myWindowManagerKeys ++ myMediaKeys

myProgramKeys =
  [ ("M-space"        , addName "Rofi menu" $ spawn "rofi -show drun -theme themes/appsmenu.rasi")
  ]

myWindowManagerKeys =
  []

myMediaKeys =
  []

-- STARTUPHOOK
myStartupHook = do
  setWMName "LG3D"
  spawn "feh -bg-scale ~/Pictures/Wallpaper/mountains.jpg"
  spawn "picom -b"
  spawn "polybar --reload top"
