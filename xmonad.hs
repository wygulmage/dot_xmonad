{-# LANGUAGE
  UnicodeSyntax
  , FlexibleInstances
  #-}

-- import Prelude hiding (take)
import XMonad
   ( X
   , Full (Full), Tall (Tall)
   , KeyMask, KeySym
   , (|||), (-->)
   , borderWidth
   , normalBorderColor, focusedBorderColor
   , modMask, mod4Mask
   , terminal
   , manageHook, layoutHook, handleEventHook, logHook, startupHook
   , spawn
   , xmonad
   )
import XMonad.Core (Query(..), WindowSet(..))
import XMonad.Config (def)
import qualified XMonad.StackSet as W -- for window management commands.

import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
   ( PP (ppLayout, ppCurrent, ppOutput, ppTitle)
   , xmobarPP
   , dynamicLogWithPP
   )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Fullscreen (fullscreenSupport, fullscreenManageHook, fullscreenEventHook)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, spawnPipe)

import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86 -- for the mOtherKeys KeySyms.
-- import Data.Semigroup ((<>))
import Numeric.Natural (Natural)


-- No Semigroup instance for some XMonad monoids ☹
(<>) :: Monoid m ⇒ m → m → m
(<>) = mappend

winKey :: KeyMask
winKey = mod4Mask -- Win key

myTerminal :: String
myTerminal = "kitty"

myOtherKeys :: [((KeyMask, KeySym), X ())]
myOtherKeys =
        ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+") :
        ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-") :
        ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle") :
        ((0, xF86XK_MonBrightnessUp), spawn "light -A 5") :
        ((0, xF86XK_MonBrightnessDown), spawn "light -U 5") :
        -- ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10 -time 0 -steps 1") :
        -- ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10 -time 0 -steps 1") :
        ((0, xF86XK_KbdBrightnessUp), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh up") :
        ((0, xF86XK_KbdBrightnessDown), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh down") :
        []


shortenWith :: [a] → Natural → [a] → [a]
shortenWith ellipsis maxLength xs
   | longer xs maxLength = take (fromIntegral maxLength - length ellipsis) xs <> ellipsis
   | otherwise = xs

longer :: [a] → Natural → Bool
longer xs n = not . null . drop (fromIntegral n) $ xs


main = do
  -- xmobar ← spawnPipe "xmobar /Ix/k/Settings/xmonad/xmobarrc"
  xmobar ← spawnPipe "xmobar ~/.config/xmonad/xmobarrc"
  xmonad . ewmh . fullscreenSupport $ def {
    borderWidth = 1, normalBorderColor = "black", focusedBorderColor = "#ff8100"
    ,
    modMask = winKey
    ,
    terminal = myTerminal
    ,
    manageHook =
       manageDocks
       <>
       (isFullscreen --> doFullFloat)
       <>
       fullscreenManageHook
       <>
       manageHook def
    ,
    layoutHook = (avoidStruts . smartBorders) (Tall 1 (3/100) (1/2) ||| Full)
    ,
    handleEventHook =
       docksEventHook
       <>
       fullscreenEventHook
       <>
       handleEventHook def
    ,
    logHook = dynamicLogWithPP xmobarPP {
       ppLayout = const ""
       ,
       ppCurrent = ("[" <>) . (<> "]")
       ,
       ppOutput = hPutStrLn xmobar
       ,
       ppTitle = shortenWith "…" 66
       } *> updatePointer (0.5, 0.5) (0.96, 0.96)
    ,
    startupHook = spawn myTerminal <> startupHook def
    } `additionalKeys` myOtherKeys

-- Restart xmonad with mod-q.
