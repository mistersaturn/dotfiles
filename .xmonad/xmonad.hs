import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ()
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Prompt.OrgMode

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W

myTerminal :: String
myTerminal = "xfce4-terminal"
myBorderWidth :: Dimension
myBorderWidth = 2
myNormalBorderColor :: String
myNormalBorderColor = "#282A36"
myFocusedBorderColor :: String
myFocusedBorderColor = "#44475A"

main :: IO ()
main = do   xmproc <- spawnPipe "xmobar"
            xmonad $ ewmh $ docks myConfig

myConfig = def
    { modMask     = mod4Mask
    , terminal    = myTerminal
    , layoutHook  = myLayout
    , manageHook  = myManageHook
    , startupHook = myStartupHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    }
    `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys =
    [("M-<Return>", spawn "xfce4-terminal")
    ,("M-C-r", spawn "xmonad --recompile; xmonad --restart")
    ,("M-b", spawn "librewolf")
    ,("M-S-b", spawn "xfce4-terminal -e links")
    ,("M-d", spawn "rofi -show drun")
    ,("M-S-o", orgPrompt def "TODO" "~/org/todos.org")
    ,("M-S-c", kill)
    ,("M-C-m", namedScratchpadAction myScratchPads "cmus")
    ,("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
    ]

myLayout = spacing 2 $ avoidStruts tiled ||| Mirror tiled ||| threeCol
    where
      tiled = Tall 1 (3/100) (1/2)
      threeCol = ThreeColMid 1 (3/100) (1/2)

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "GIMP" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop" --> doIgnore
    ] <+> namedScratchpadManageHook myScratchPads

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus
                ]

    where
    spawnTerm = myTerminal ++ " -T scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCmus = myTerminal ++ " -T cmus -e 'cmus'"
    findCmus = title =? "cmus"
    manageCmus = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

myStartupHook = do
    spawnOnce "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal &"
    spawnOnce "compton &"
    spawnOnce "nitrogen --restore &"
