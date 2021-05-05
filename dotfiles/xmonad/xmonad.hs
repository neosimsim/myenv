import Graphics.X11.ExtraTypes
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook (NoUrgencyHook (..), focusUrgent, withUrgencyHook)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)

main :: IO ()
main = do
  xmonad
    =<< ( myStatusBar
            . withUrgencyHook NoUrgencyHook
            . fullscreenSupport
            $ myConfig `additionalKeys` myKeys
        )

myModMask :: KeyMask
myModMask = mod4Mask

myConfig =
  def
    { borderWidth = 2,
      modMask = myModMask,
      terminal = "urxvt",
      manageHook =
        composeAll
          [ manageHook def,
            manageDocks,
            stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "3",
            className =? "Gimp" --> doFloat,
            className =? "Signal" --> doShift "9",
            className =? "Wire" --> doShift "9",
            className =? "discord" --> doShift "9",
            className =? "Slack" --> doShift "9",
            className =? "pavucontrol" <||> className =? "Pavucontrol"
              --> doCenterFloat
          ],
      layoutHook =
        smartBorders . avoidStruts $ Tall 1 (1 / 300) (3 / 4) ||| Full ||| Grid,
      handleEventHook = handleEventHook def <+> docksEventHook,
      normalBorderColor = "#cccccc",
      focusedBorderColor = "#cd8b00",
      startupHook = setWMName "LG3D",
      focusFollowsMouse = False
    }

myStatusBar =
  statusBar
    "xmobar"
    ( xmobarPP
        { ppTitle = shorten 50,
          ppCurrent = wrap "[" "]",
          ppUrgent = wrap "." "."
        }
    )
    (\XConfig {modMask = modm} -> (modm, xK_b))

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((myModMask, xK_Down), windows W.focusDown), -- %! Move focus to the next window
    ((myModMask, xK_Up), windows W.focusUp), -- %! Move focus to the previous window
    ((myModMask .|. shiftMask, xK_Down), windows W.swapDown), -- %! Swap the focused window with the next window
    ((myModMask .|. shiftMask, xK_Up), windows W.swapUp), -- %! Swap the focused window with the previous window
    ((myModMask, xK_Left), sendMessage Shrink), -- %! Shrink the master area
    ((myModMask, xK_Right), sendMessage Expand), -- %! Expand the master area
    ((myModMask, xK_BackSpace), focusUrgent),
    ((myModMask .|. shiftMask, xK_p), spawn "dpass"),
    ((myModMask .|. shiftMask, xK_l), spawn "slock"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-"),
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+"),
    ((0, xF86XK_AudioMute), spawn "amixer set Master toggle"),
    ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +10%"),
    ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-"),
    ((myModMask, xK_Print), spawn "scrot -s"),
    ((0, xK_Print), spawn "scrot"),
    ((myModMask, xK_a), spawn "rotatekb"),
    ((myModMask, xK_g), spawn "games"),
    ((myModMask, xK_Arabic_sheen), spawn "rotatekb")
  ]
