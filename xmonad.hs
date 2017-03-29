import XMonad
import XMonad.Config (def)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
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
        ((0, xF86XK_KbdBrightnessUp), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh up") :
        ((0, xF86XK_KbdBrightnessDown), spawn "/Ix/k/Settings/xmonad/kbd-backlight.sh down") :
        []

-- myCurrentLayout = ask >>= \w -> liftX $ do
--   d <- asks display
--   let layoutName = description . W.layout . W.workspace . W.current


main = do
  xmproc <- spawnPipe "xmobar /Ix/k/Settings/xmonad/xmobarrc"
  xmonad $ def {
    borderWidth = 1,
    normalBorderColor = "black",
    focusedBorderColor = "#ff8100",
    modMask = myModKey,
    terminal = myTerminal,
    manageHook = manageDocks <+> (className =? "smplayer" --> doFullFloat) <+> (isFullscreen --> doFullFloat) <+> manageHook def, -- to make this work properly I need another Query that gets the current layout and compares it to "Full". Hints: liftX, ask, description . W.layout . W.workspace . W.current.
    layoutHook = avoidStruts $ smartBorders $ (Tall 1 (3/100) (1/2)) ||| Full,
    handleEventHook = docksEventHook `mappend` handleEventHook def,
    logHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc,
        ppTitle = shorten 50
        },
    startupHook = (spawn myTerminal) <+> startupHook def
    } `additionalKeys` myOtherKeys

