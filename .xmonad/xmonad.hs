import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Xfce
main = xmonad $ xfceConfig {
	terminal = "xfce4-terminal",
	modMask = mod4Mask
	}
	`additionalKeysP`
	[ ("M-i", spawn "firefox" )
	, ("M-e", spawn "emacsclient -ca \"\"" ) ]
