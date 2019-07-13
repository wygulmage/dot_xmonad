{-# LANGUAGE
  UnicodeSyntax
  , FlexibleInstances
  -- , NoImplicitPrelude
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
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
   ( ppLayout, ppCurrent, ppOutput, ppTitle
   , dynamicLogWithPP, xmobarPP
   -- , shorten
   , wrap
   )
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docksEventHook)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, spawnPipe)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Fullscreen (fullscreenSupport, fullscreenManageHook, fullscreenEventHook)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W -- for window management commands.
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86 -- for the mOtherKeys KeySyms.
-- import Data.Semigroup
import Numeric.Natural (Natural)


-- No Semigroup instance for some XMonad monoids.
(<>) :: Monoid m ⇒ m → m → m
(<>) = mappend

winKey :: KeyMask
winKey = mod4Mask -- Win key

myTerminal :: String
myTerminal = "kitty" --was "cool-retro-term"

myOtherKeys :: [((KeyMask, KeySym), X ())]
myOtherKeys =
        ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+") :
        ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-") :
        ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle") :
        -- ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 8") :
        ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10 -time 0 -steps 1") :
        -- ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 8") :
        ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10 -time 0 -steps 1") :
        ((0, xF86XK_KbdBrightnessUp), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh up") :
        ((0, xF86XK_KbdBrightnessDown), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh down") :
        []

shorten :: Natural → String → String
shorten = shortenWith "…"

shortenWith :: [a] → Natural → [a] → [a]
shortenWith ellipsis maxLength xs
   | length xs > fromIntegral maxLength = take (fromIntegral maxLength - length ellipsis) xs <> ellipsis
   | otherwise = xs


main = do
  xmobar ← spawnPipe "xmobar /Ix/k/Settings/xmonad/xmobarrc"
  xmonad . fullscreenSupport $ def {
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
        ppCurrent = wrap "[" "]"
        ,
        ppOutput = hPutStrLn xmobar
        ,
        ppTitle = shorten 60
        } *> updatePointer (0.5, 0.5) (0.96, 0.96)
    ,
    startupHook = spawn myTerminal <> startupHook def
    } `additionalKeys` myOtherKeys

-- Restart xmonad with mod-q.
