import XMonad
import System.Exit
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Prompt.OrgMode (orgPrompt)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "xfce4-terminal"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 2
myNormalBorderColor = "#282A36"
myFocusedBorderColor = "#44475A"

myModMask = mod4Mask

myWorkspaces = ["1","2","3","4","5","6"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_d     ), spawn "rofi -show drun")
    , ((modm .|. shiftMask, xK_d     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_o     ), orgPrompt def "TODO" "~/org/todos.org")
    , ((modm .|. controlMask, xK_m   ), namedScratchpadAction myScratchPads "cmus")
    , ((modm .|. controlMask, xK_Return), namedScratchpadAction myScratchPads "terminal")
    , ((modm,               xK_b     ), spawn "librewolf")
    , ((modm .|. shiftMask, xK_b     ), spawn "xfce4-terminal -e links")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_p), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1..xK_6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = avoidStruts tiled ||| Mirror tiled ||| Full
  where
     tiled = Tall nmaster delta ratio
     nmaster = 1
     ratio = 1/2
     delta = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "GIMP"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ] <+> namedScratchpadManageHook myScratchPads

myEventHook = mempty
myLogHook = return ()
myStartupHook = do
              spawnOnce "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal &"
              spawnOnce "compton &"
              spawnOnce "nitrogen --restore &"

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

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks defaults

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
