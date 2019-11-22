import XMonad

main = xmonad defaultConfig
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = myBorderWidth
    }

myTerminal        = "urxvt"
myModMask          = mod4Mask
myBorderWidth     = 3
