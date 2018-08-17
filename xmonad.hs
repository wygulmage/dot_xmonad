{-# LANGUAGE FlexibleInstances #-}

import XMonad -- (spawn, spawnPipe)
import XMonad.Core (Query(..), WindowSet(..))
import XMonad.Config (def)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenSupport, fullscreenManageHook, fullscreenEventHook)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W -- for window management commands.
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86
import Data.Semigroup


winKey = mod4Mask -- Win key
myTerminal = "cool-retro-term"
myOtherKeys =
        ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+") :
        ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-") :
        ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle") :
        ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 8") :
        ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 8") :
        ((0, xF86XK_KbdBrightnessUp), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh up") :
        ((0, xF86XK_KbdBrightnessDown), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh down") :
        []


main = do
  xmproc <- spawnPipe "xmobar /Ix/k/Settings/xmonad/xmobarrc"
  (xmonad . fullscreenSupport) $ def {
    borderWidth = 1, normalBorderColor = "black", focusedBorderColor = "#ff8100"
    ,
    modMask = winKey
    ,
    terminal = myTerminal
    ,
    manageHook =
      manageDocks `mappend`
      (isFullscreen --> doFullFloat) `mappend`
      fullscreenManageHook `mappend`
      manageHook def
    ,
    layoutHook = (avoidStruts . smartBorders) (Tall 1 (3/100) (1/2) ||| Full)
    ,
    handleEventHook =
      docksEventHook `mappend`
      fullscreenEventHook `mappend`
      handleEventHook def
    ,
    logHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc
        ,
        ppTitle = shorten 50
        } >> updatePointer (0.5, 0.5) (1, 1)
    ,
    startupHook = spawn myTerminal `mappend` startupHook def
    } `additionalKeys` myOtherKeys

-- Restart xmonad with mod-q.
