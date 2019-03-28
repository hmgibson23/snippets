{-# LANGUAGE UnicodeSyntax #-}
 -- Libraries {{{
import Control.Monad
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Square
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiColumns

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import qualified XMonad.StackSet as W
-- }}}

-- Main module {{{
main = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    safeSpawn "touch" ["/tmp/" ++ file]
  wsPanel     <- spawnPipe wsBar
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ ewmh defaultConfig
      -- Simple stuff
      { modMask             = modm
      , terminal            = term
      , focusFollowsMouse   = mouseFocus
      , borderWidth         = bdrSize
      , normalBorderColor   = bdrNormal
      , focusedBorderColor  = bdrFocus
      , workspaces          = workspaces'

      -- Lets hook up
      , handleEventHook     = eventHook
      , logHook             = logHook' wsPanel
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , startupHook         = startupHook'
      } `additionalKeys` keyboard

---- Simple stuff
modm          = mod4Mask
term          = "st -e zsh"
mouseFocus    = False
workspaces'   = myWorkspaces
keyboard      = myKeys
browser       = "surf"

----- Appearance
bdrSize       = 0
bdrNormal     = fgColor
bdrFocus      = bgColor
font          = "SauceCodePro Nerd Font:size=9:antialias=true:autohint=true"
monitorSize   = 1980
monitor n     = show(round(monitorSize * n))
monitor' n    = round(monitorSize * n)

----- WHAT COLOR?
bgColor       = "#4f4847"
fgColor       = "#F2EBEA"
layoutColor   = "#AA3355"
wsBgColor     = "#f1f3f4"
wsFgColor     = bgColor
barBgColor    = bgColor
barFgColor    = fgColor
hintColor     = layoutColor

---- Panel
leftBarSize   = monitor 1
leftBarPos    = "0"
barHeight     = "26"

---- Hooks
eventHook     = fullscreenEventHook
layoutHook'   = myLayoutHook
logHook'      = myLogHook
manageHook'   = myManageHook
-- namedScratchpadManageHook scratchpads <+>
startupHook'  = myStartupHook
-- }}}

-- Dzen2 {{{
wsBar      =
  "dzen2 -dock -ta l      \
  \ -bg '" ++ barBgColor  ++ "' \
  \ -fg '" ++ barFgColor  ++ "' \
  \ -w  '" ++ leftBarSize ++ "' \
  \ -h  '" ++ barHeight   ++ "' \
  \ -x  '" ++ leftBarPos  ++ "' \
  \ -fn '" ++ font        ++ "' "
-- }}}

-- Log Hook {{{
myLogHook h =
  dynamicLogWithPP $
  dzenPP
    { ppOutput  = hPutStrLn h
    , ppCurrent = dzenColor (fg) (bg) . pad
    , ppVisible = pad
    , ppHidden  = pad . noScratchPad
    , ppUrgent  = dzenColor (bg) (hint) . pad
    , ppSep     = ""
    , ppOrder   = \(ws:l:t:_) -> [l, ws]
    , ppLayout  = dzenColor (bg) (layoutBg) . pad . pad .
        ( \t -> case t of
          "Tall" -> "þ"
          "Mirror Tall" -> "ü"
          "Full" -> "ÿ"
          "Accordion" -> "\f037"
          _ -> t
        )
    }
  where
    bg = wsBgColor
    fg = wsFgColor
    hint = hintColor
    layoutBg = layoutColor
    noScratchPad ws = if ws == "NSP" then "" else ws
-- }}}

-- Workspaces {{{
myWorkspaces = ["1", "2", "DEV", "4",  "5", "WORK", "MAIL", "CHAT", "INET"]
-- }}}


-- Layouts {{{
standardLayout = smartBorders $
  renamed [CutWordsLeft 1] $
  smartSpacingWithEdge 8 $ layoutHook defaultConfig
tallLayout = named "tall" $ avoidStruts $ standardLayout
wideLayout = named "wide" $ avoidStruts $ Mirror standardLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
termLayout = gaps [(L,50), (U,50), (R,50), (D,50)] $ standardLayout
gridLayout = named "grid" $ gaps [(L,50), (U,50), (R,50), (D,50)] $ Grid
extra = simpleTabbed ||| simpleFloat ||| Accordion

myLayoutHook =
  avoidStruts
  $ mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ onWorkspace (w !! 0) termLayout
  $ onWorkspace (w !! 7) termLayout
  $ standardLayout ||| normal ||| extra
  where
    w = workspaces'
    normal = tallLayout ||| wideLayout ||| singleLayout ||| fullscreenLayout

-- }}}

-- Manage Hook {{{
myManageHook =
    composeAll . concat $
    [
      [ className =? c --> doShift (w !! 8) | c <- inetApp ]
    , [ className =? c --> doShift (w !! 2) | c <- devApp ]
    , [ className =? c --> doShift (w !! 4) | c <- entApp ]
    , [ className =? c --> doShift (w !! 5) | c <- prodApp ]
    , [ className =? c --> doShift (w !! 6) | c <- mailApp ]
    , [ className =? c --> doShift (w !! 7) | c <- chatApp ]
    , [ className =? c --> doFloat          | c <- floatingApp ]
    , [ className =? c --> doIgnore         | c <- ignoreApp ]
    , [ isDialog       --> doCenterFloat ]
    , [ isRole         --> doCenterFloat ]
    , [ manageDocks ]
    , [ manageHook def ]
    ]
    where
      w = workspaces'
      isRole = stringProperty "WM_WINDOW_ROLE" =? "pop-up"
      inetApp = ["Chromium", "Firefox", "Transmission"]
      chatApp = ["Slack", "Keybase"]
      devApp =
        [ "VirtualBox Manager", "VirtualBox Machine", "Emacs"]
      entApp = ["MPlayer", "smplayer", "mpv", "Gimp", "VLC"]
      prodApp = ["zathura", "Audacity", "Calibre"]
      mailApp = ["mail"]
      floatingApp = ["SecureCRT", "TeamViewer", "Xmessage"]
      ignoreApp = ["desktop", "desktop_window", "stalonetray", "trayer"]
  -- }}}

-- Scratchpad {{{
scratchpads = [
    NS "bash" "st -e bash" (title =? "bash") defaultFloating ,
    NS "mksh" "st -e mksh" (title =? "mksh") defaultFloating ,
    NS "surf" "surf" (title =? "surf") defaultFloating ]
      where role = stringProperty "WM_WINDOW_ROLE"
-- }}}


-- Startup Hook {{{
myStartupHook = do
  spawnOnce "$HOME/.xmonad/autostart.sh"
  spawnOnce "xsetroot -cursor_name left_ptr"
-- }}}

-- Keymapping {{{
-- /usr/include/X11/keysymdef.h
myKeys =
  [ ((m, xK_b), spawn browser)
  , ((m, xK_p), spawn rofi)
  , ((m .|. s, xK_l), spawn slock)
  , ((m .|. s, xK_n),  nextWS)
  , ((m .|. s, xK_e),    prevWS)
  , ((m, xK_n), windows W.focusDown)
  , ((m, xK_e), windows W.focusUp  )
  , ((m, xK_y), sendMessage NextLayout)
  , ((m, xK_s), sendMessage ToggleStruts)
  , ((m, xK_r), restart "xmonad" True)
  , ((m, xK_q), kill)
  , ((m, xK_BackSpace), focusUrgent)
  , ((m, xK_equal), toggleWS)
  , ((m, xK_grave), toggleWS)
  , ((m, xK_f), sendMessage $ Toggle FULL)
  , ((m .|. c , xK_m), namedScratchpadAction scratchpads "mksh")
  , ((m .|. c , xK_b), namedScratchpadAction scratchpads "bash")
  , ((m .|. c , xK_s), namedScratchpadAction scratchpads "surf")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+")
  , ((0, xF86XK_AudioMute          ), spawn "amixer sset Master toggle")
  , ((0, xF86XK_MonBrightnessDown), spawn "light -U 10")
  , ((0, xF86XK_MonBrightnessUp), spawn "light -A 10")
  ]
  where
    m = modm
    s = shiftMask
    c = controlMask
    slock = "slock"
    rofi = "$HOME/.xmonad/rofi.sh"
    dmenu =
      "dmenu_run -i \
      \ -fn '" ++ fn ++ "' \
      \ -nf '" ++ fgColor ++ "' \
      \ -sf '" ++ fgColor ++ "' \
      \ -nb '" ++ bgColor ++ "' \
      \ -sb '" ++ layoutColor ++ "'"
      where
        fn = "Misc Termsyn.Icons:size=18"
-- }}}
