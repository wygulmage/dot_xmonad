import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /Ix/keith/Settings/xmonad/xmobarrc"
  xmonad defaultConfig {
    borderWidth = 1,
    normalBorderColor = "black",
    focusedBorderColor = "#7777AA",
    modMask = mod4Mask, -- 'Win' key
    terminal = "cool-retro-term",
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig,
    handleEventHook = docksEventHook `mappend` handleEventHook defaultConfig
    }
