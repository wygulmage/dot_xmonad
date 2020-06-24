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
   , manageHook, layoutHook, logHook, startupHook
   , xmonad
   )
-- import XMonad.Core (Query(..), WindowSet)
import XMonad.Config (def)
-- import qualified XMonad.StackSet as W -- for window management commands.

import XMonad.Actions.WindowGo (ifWindow)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
   ( PP (ppLayout, ppCurrent, ppOutput, ppTitle)
   , xmobarPP
   , dynamicLogWithPP
   )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.ManageHook ((=?), className, idHook)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, spawnPipe)

import Control.Monad.IO.Class (MonadIO)
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86 -- for the mOtherKeys KeySyms.
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)


winKey :: KeyMask
winKey = mod4Mask -- Win key

myTerminal :: String
myTerminal = "kitty"

mySpawn :: MonadIO m => FilePath -> String -> m ()
mySpawn prog = safeSpawn prog . words

myOtherKeys :: [((KeyMask, KeySym), X ())]
myOtherKeys =
        ((0, xF86XK_AudioRaiseVolume), mySpawn "amixer" "set Master 2%+") :
        ((0, xF86XK_AudioLowerVolume), mySpawn "amixer" "set Master 2%-") :
        ((0, xF86XK_AudioMute), mySpawn "amixer" "-D pulse set Master toggle") :
        ((0, xF86XK_MonBrightnessUp), mySpawn "light" "-A 5") :
        ((0, xF86XK_MonBrightnessDown), mySpawn "light" "-U 5") :
        -- ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10 -time 0 -steps 1") :
        -- ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10 -time 0 -steps 1") :
        ((0, xF86XK_KbdBrightnessUp), mySpawn "/Ix/k/Settings/xmonad/kbd-backlight.sh" "up") :
        ((0, xF86XK_KbdBrightnessDown), mySpawn "/Ix/k/Settings/xmonad/kbd-backlight.sh" "down") :
        []


shortenWith :: [a] → Natural → [a] → [a]
-- Warning: If ellipsis is longer than maxLength, this will return lists that are longer than maxLength.
shortenWith ellipsis maxLength xs
   | longer xs maxLength = take (fromIntegral maxLength - length ellipsis) xs <> ellipsis
   | otherwise = xs

longer :: [a] → Natural → Bool
longer xs n = not . null . drop (fromIntegral n) $ xs

myForegroundColor, myBackgroundColor :: String
myForegroundColor = "#ff8100"
myBackgroundColor = "black"

main :: IO ()
main = do
  xmobar ← spawnPipe "xmobar ~/.config/xmonad/xmobarrc"
  -- Looks like we need to apply fullscreenSupport before docks to get borderless full screen.
  xmonad . ewmh . docks . fullscreenSupport $ def{
    borderWidth = 1, normalBorderColor = myBackgroundColor, focusedBorderColor = myForegroundColor
    ,
    modMask = winKey
    ,
    terminal = myTerminal
    ,
    manageHook = (isFullscreen --> doFullFloat) <> manageHook def -- no border on fullscreen windows
    ,
    layoutHook = (avoidStruts . smartBorders) (Tall 1 (3/100) (1/2) ||| Full)
    ,
    logHook = dynamicLogWithPP xmobarPP{
       ppLayout = const ""
       ,
       ppCurrent = ("[" <>) . (<> "]")
       ,
       ppOutput = hPutStrLn xmobar
       ,
       ppTitle = shortenWith "…" 66
       }
       *> updatePointer (0.5, 0.5) (0.96, 0.96)
    ,
    startupHook = ifWindow (className =? myTerminal) idHook (mySpawn myTerminal "") <> startupHook def -- If there's no terminal open, open it.
    }
    `additionalKeys` myOtherKeys

-- Restart xmonad with mod-q.
