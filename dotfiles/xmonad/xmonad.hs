import Graphics.X11.ExtraTypes
import System.IO (hPutStrLn)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -o $HOME/.xmobarrc"
  xmonad . docks . fullscreenSupport $ myConfig xmproc `additionalKeys` myKeys
  where
    myConfig xmproc =
      def
        { borderWidth = 2,
          modMask = myModMask,
          terminal = "urxvt",
          manageHook =
            composeAll
              [ manageHook def,
                manageDocks,
                className =? "Gimp" --> doFloat,
                className =? "Chromium" --> doShift "3",
                className =? "Firefox" --> doShift "3",
                className =? "signal" --> doShift "9",
                className =? "wire" --> doShift "9",
                className =? "discord" --> doShift "9"
              ],
          layoutHook =
            smartBorders . avoidStruts $ Grid ||| Full ||| Tall 1 (1 / 300) (1 / 2),
          handleEventHook = handleEventHook def <+> docksEventHook,
          logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppTitle = shorten 50,
                  ppCurrent = \x -> "[" ++ x ++ "]"
                },
          normalBorderColor = "#cccccc",
          focusedBorderColor = "#cd8b00",
          startupHook = setWMName "LG3D",
          focusFollowsMouse = False
        }

myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((myModMask, xK_Down), windows W.focusDown), -- %! Move focus to the next window
    ((myModMask, xK_Up), windows W.focusUp), -- %! Move focus to the previous window
    ((myModMask .|. shiftMask, xK_Down), windows W.swapDown), -- %! Swap the focused window with the next window
    ((myModMask .|. shiftMask, xK_Up), windows W.swapUp), -- %! Swap the focused window with the previous window
    ((myModMask, xK_Left), sendMessage Shrink), -- %! Shrink the master area
    ((myModMask, xK_Right), sendMessage Expand), -- %! Expand the master area
    ((myModMask .|. shiftMask, xK_p), spawn "dpass"),
    ((myModMask .|. shiftMask, xK_l), spawn "slock"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-"),
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+"),
    ((0, xF86XK_AudioMute), spawn "amixer set Master toggle"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10"),
    ((myModMask, xK_Print), spawn "scrot -s"),
    ((0, xK_Print), spawn "scrot"),
    ((myModMask, xK_a), spawn "rotatekb"),
    ((myModMask, xK_g), spawn "games"),
    ((myModMask, xK_Arabic_sheen), spawn "rotatekb")
  ]
