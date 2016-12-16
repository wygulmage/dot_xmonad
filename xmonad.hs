import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)

main = do
  xmproc <- spawnPipe "xmobar /Ix/keith/Settings/xmonad/xmobarrc"
  xmonad defaultConfig {
  modMask = mod4Mask, -- 'Win' key
  terminal = "cool-retro-term",
  manageHook = manageDocks <+> manageHook defaultConfig,
  layoutHook = avoidStruts $ layoutHook defaultConfig
  }
