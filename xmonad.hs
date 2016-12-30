import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W -- for window management commands.
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86

myModKey = mod4Mask -- Win key
myTerminal = "cool-retro-term"
myOtherKeys =
        ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+") :
        ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-") :
        ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle") :
        ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 8") :
        ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 8") :
        ((0, xF86XK_KbdBrightnessUp), spawn "/Ix/keith/Settings/xmonad/kbd-backlight.sh up") :
        ((0, xF86XK_KbdBrightnessDown), spawn "/Ix/keith/Settings/xmonad/kbd-backlight.sh down") :
        []

main = do
  xmproc <- spawnPipe "xmobar /Ix/keith/Settings/xmonad/xmobarrc"
  xmonad $ defaultConfig {
    borderWidth = 1,
    normalBorderColor = "black",
    focusedBorderColor = "#ff8100",
    modMask = myModKey,
    terminal = myTerminal,
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig,
    handleEventHook = docksEventHook `mappend` handleEventHook defaultConfig,
    logHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc,
        ppTitle = shorten 50
        },
    startupHook = (spawn myTerminal) <+> startupHook defaultConfig
    } `additionalKeys` myOtherKeys
