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
import XMonad.Config (def)
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

import Graphics.X11.ExtraTypes.XF86 -- for the KeySyms.

import Control.Monad.IO.Class (MonadIO)
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as Map
import System.IO (hPutStrLn)


winKey :: KeyMask
winKey = mod4Mask -- Win key

myTerminal :: String
myTerminal = "kitty"

myForegroundColor, myBackgroundColor :: String
-- myForegroundColor = "#ff8100"
myForegroundColor = "#337ad9"
myBackgroundColor = "#000000"

myKeys :: Map.Map (ButtonMask, KeySym) (X ())
myKeys =
    Map.fromList (
        ((0, xF86XK_AudioRaiseVolume), mySpawn "amixer" "set Master 2%+") :
        ((0, xF86XK_AudioLowerVolume), mySpawn "amixer" "set Master 2%-") :
        ((0, xF86XK_AudioMute), mySpawn "amixer" "-D pulse set Master toggle") :
        -- ((0, xF86XK_MonBrightnessUp), mySpawn "light" "-A 5") :
        -- ((0, xF86XK_MonBrightnessDown), mySpawn "light" "-U 5") :
        ((0, xF86XK_KbdBrightnessUp), mySpawn "/Ix/k/Settings/xmonad/kbd-backlight.sh" "up") :
        ((0, xF86XK_KbdBrightnessDown), mySpawn "/Ix/k/Settings/xmonad/kbd-backlight.sh" "down") :
        [])


main :: IO ()
main = do
    xmobar <- spawnPipe "xmobar ~/.config/xmonad/xmobarrc"
    -- Apply fullscreenSupport before docks to get borderless full screen.
    xmonad . ewmhFullscreen . ewmh . docks . fullscreenSupport $ def
        { modMask = winKey
        , terminal = myTerminal
        , borderWidth = 1
        , normalBorderColor = myBackgroundColor
        , focusedBorderColor = myForegroundColor
        , layoutHook = (avoidStruts . smartBorders) (Tall 1 (3/100) (1/2) ||| Full)
        , manageHook =
            (isFullscreen --> doFullFloat) <> -- no border on fullscreen windows
            (className =? "Nautilus" --> doShift "2") <>
            (className =? "Pale moon" --> doShift "3") <>
            manageHook def
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

--- Utilities ---

mySpawn :: MonadIO m => FilePath -> String -> m ()
mySpawn prog = safeSpawn prog . words
