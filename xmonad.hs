{-# LANGUAGE
  UnicodeSyntax
  , FlexibleInstances
  #-}

-- import Prelude hiding (take)
import XMonad
   ( X
   , Full (Full), Tall (Tall)
   , ButtonMask, KeyMask, KeySym
   , (|||), (-->)
   , borderWidth
   , normalBorderColor, focusedBorderColor
   , modMask, mod4Mask
   , terminal
   , keys
   , manageHook, layoutHook, logHook, startupHook
   , xmonad
   )
-- import XMonad.Core (Query(..), WindowSet)
import XMonad.Config (def)
-- import qualified XMonad.StackSet as W -- for window management commands.
import XMonad.ManageHook ((=?), className, idHook, doShift)

import XMonad.Actions.WindowGo (ifWindow)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
   ( PP (ppLayout, ppCurrent, ppOutput, ppTitle)
   , xmobarPP
   , dynamicLogWithPP
   )
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (safeSpawn, spawnPipe)

import Control.Monad.IO.Class (MonadIO)
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86 -- for the mOtherKeys KeySyms.
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)


winKey :: KeyMask
winKey = mod4Mask -- Win key

myTerminal :: String
myTerminal = "kitty"

mySpawn :: MonadIO m => FilePath -> String -> m ()
mySpawn prog = safeSpawn prog . words

myKeys :: Map.Map (ButtonMask, KeySym) (X ())
myKeys =
   Map.fromList (
      ((0, xF86XK_AudioRaiseVolume), mySpawn "amixer" "set Master 2%+") :
      ((0, xF86XK_AudioLowerVolume), mySpawn "amixer" "set Master 2%-") :
      ((0, xF86XK_AudioMute), mySpawn "amixer" "-D pulse set Master toggle") :
      ((0, xF86XK_MonBrightnessUp), mySpawn "light" "-A 5") :
      ((0, xF86XK_MonBrightnessDown), mySpawn "light" "-U 5") :
      ((0, xF86XK_KbdBrightnessUp), mySpawn "/Ix/k/Settings/xmonad/kbd-backlight.sh" "up") :
      ((0, xF86XK_KbdBrightnessDown), mySpawn "/Ix/k/Settings/xmonad/kbd-backlight.sh" "down") :
      [])


shortenWith :: [a] → Natural → [a] → [a]
-- Warning: If ellipsis is longer than maxLength, this will return lists that are longer than maxLength.
shortenWith ellipsis maxLength xs
   | longer xs maxLength =
       take (fromIntegral maxLength - length ellipsis) xs <> ellipsis
   | otherwise = xs

longer :: [a] → Natural → Bool
longer xs n = not . null . drop (fromIntegral n) $ xs

myForegroundColor, myBackgroundColor :: String
myForegroundColor = "#ff8100"
myBackgroundColor = "black"

myXmobarConfig = -- haven't gotten this working yet...
   "-v " <>
   " -f xft:Source Code Pro:size=14,Symbola:size=14" <>
   " -B " <> myBackgroundColor <>
   " -F " <> myForegroundColor <>
   " --top " <>
   " -t '%StdinReader% } %date% { %kbd% %alsa:default:Master% %wi% %battery%'" <>
   " -c '[ Run StdinReader" <>
    ", Run Date \"%A %B %-d %_H:%M\" \"date\" 600" <>
    ", Run Kbd [ (\"us(dvorak)\", \"DV\"), (\"us\", \"US\") ]" <>
    ", Run Alsa \"default\" \"Master\"" <>
               "[ \"-t\", \"<action=`(amixer -D pulse set Master toggle &)`><status><volume></action>\"," <>
                 "\"--\"," <>
                 "\"-O\", \"<fn=1>📢</fn>\"," <>
                 "\"-C\",\"" <> myForegroundColor <> "\"," <>
                 "\"-o\", \"<fn=1>📢̸</fn>\"," <>
                 "\"-c\", \"#909190\"" <>
               "]" <>
    ", Run Battery [ \"-t\", \"<acstatus>\"," <>
                    "\"--\"," <>
                    "\"-O\", \"<fn=1>🔌</fn><left>\"," <>
                    "\"-i\", \"<fn=1>🔌</fn>\"," <>
                    "\"-o\", \"<fn=1>☢</fn><left>\"" <>
                  "] 10" <>
    ", Run Wireless [] [ \"-t\", \"<action=`(nm-connection-editor &)` button=1><fn=1>📡</fn><ssid></action>\" ] 60" <>
    "]'"

main :: IO ()
main = do
  xmobar ← spawnPipe "xmobar ~/.config/xmonad/xmobarrc"
  -- Looks like we need to apply fullscreenSupport before docks to get borderless full screen.
  xmonad . ewmhFullscreen . ewmh . docks . fullscreenSupport $ def
     { borderWidth = 1
     , normalBorderColor = myBackgroundColor
     , focusedBorderColor = myForegroundColor
     , modMask = winKey
     , terminal = myTerminal
     , manageHook =
        (isFullscreen --> doFullFloat) <>
        (className =? "Nautilus" --> doShift "2") <>
        (className =? "Pale moon" --> doShift "3") <>
        manageHook def -- no border on fullscreen windows
     , layoutHook = (avoidStruts . smartBorders) (Tall 1 (3/100) (1/2) ||| Full)
     , logHook = dynamicLogWithPP xmobarPP
        { ppLayout = const ""
        , ppCurrent = ("[" <>) . (<> "]")
        , ppOutput = hPutStrLn xmobar
        , ppTitle = id
        }
        *> updatePointer (0.5, 0.5) (0.96, 0.96)
     , startupHook =
        ifWindow (className =? myTerminal) idHook (mySpawn myTerminal "") <> -- If there's no terminal open, open it.
        ifWindow (className =? "Nautilus") idHook (mySpawn "nautilus" "") <>
        ifWindow (className =? "Pale moon") idHook (mySpawn "palemoon" "--private") <>
        startupHook def
     , keys = Map.union myKeys . keys def -- keys is a function from an XConfig to a Map of keys so it can grab modMask from the XConfig.
     }

-- Restart xmonad with mod-q.
