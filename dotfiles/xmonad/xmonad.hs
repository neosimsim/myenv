import Distribution.SPDX (LicenseId (FSFAP))
import Graphics.X11.ExtraTypes
import XMonad
import XMonad.Actions.WindowGo (raise)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.Rescreen (addRandrChangeHook)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook (NoUrgencyHook (..), focusUrgent, withUrgencyHook)
import XMonad.Layout.Decoration (Theme (windowTitleAddons))
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Prompt (XPConfig (height, searchPredicate, sorter, autoComplete), XPrompt, showXPrompt)
import XMonad.Prompt.AppLauncher (mkXPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.StackSet (allWindows)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, safeSpawnProg)

main :: IO ()
main =
  xmonad
    =<< ( myStatusBar
            . withUrgencyHook NoUrgencyHook
            . fullscreenSupport
            . docks
            . addRandrChangeHook (safeSpawnProg "setscreens")
            . (`additionalKeys` myKeys)
            $ myConfig
        )

myModMask :: KeyMask
myModMask = mod4Mask

myConfig =
  def
    { borderWidth = 2,
      modMask = myModMask,
      terminal = "alacritty",
      manageHook =
        composeAll
          [ manageHook def,
            stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "3",
            title =? "Ediff" --> doFloat,
            className =? "Gimp" --> doFloat,
            className =? "Signal" --> doShift "9",
            className =? "Wire" --> doShift "9",
            className =? "discord" --> doShift "9",
            className =? "Slack" --> doShift "9",
            className =? "Microsoft Teams - Preview" --> doShift "5"
          ],
      layoutHook =
        smartBorders . avoidStruts $ Tall 1 (1 / 8) (1 / 2) ||| Full,
      normalBorderColor = "#cccccc",
      focusedBorderColor = "#cd8b00",
      startupHook = myStartupHook,
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
    ((myModMask, xK_q), spawn "if type xmonad; then xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"), -- foo Restart xmonad same as default key binding but no recompile. Home Manager recompiles already.
    ((myModMask, xK_BackSpace), focusUrgent),
    ((myModMask .|. shiftMask, xK_p), safeSpawnProg "dpass"),
    ((myModMask .|. shiftMask, xK_l), safeSpawnProg "xlock"),
    ((0, xF86XK_AudioLowerVolume), safeSpawn "amixer" ["set", "Master", "2%-"]),
    ((0, xF86XK_AudioRaiseVolume), safeSpawn "amixer" ["set", "Master", "2%+"]),
    ((0, xF86XK_AudioMute), safeSpawn "amixer" ["set", "Master", "toggle"]),
    ((shiftMask, xF86XK_AudioLowerVolume), safeSpawn "amixer" ["set", "Capture", "2%-"]),
    ((shiftMask, xF86XK_AudioRaiseVolume), safeSpawn "amixer" ["set", "Capture", "2%+"]),
    ((shiftMask, xF86XK_AudioMute), safeSpawn "amixer" ["set", "Capture", "toggle"]),
    ((0, xF86XK_MonBrightnessUp), safeSpawn "brightnessctl" ["set", "+10%"]),
    ((0, xF86XK_MonBrightnessDown), safeSpawn "brightnessctl" ["set", "10%-"]),
    ((myModMask, xK_Print), safeSpawn "scrot" ["-s"]),
    ((0, xK_Print), safeSpawnProg "scrot"),
    ((myModMask, xK_a), safeSpawnProg "rotatekb"),
    ((myModMask, xK_g), safeSpawnProg "games"),
    ((myModMask, xK_Arabic_sheen), safeSpawnProg "rotatekb"),
    ((myModMask, xK_w), focusApplicationPromp)
  ]

myStartupHook = do
  startupHook def
  setWMName "LG3D"

data FocusApplicationPrompt = FAP

instance XPrompt FocusApplicationPrompt where
  showXPrompt FAP = "Focus Window: "

focusApplicationPromp :: X ()
focusApplicationPromp = do
  titles <- windowTitles
  let xpConfig =
        def
          { height = 30,
            searchPredicate = fuzzyMatch,
            sorter = fuzzySort
          }

  mkXPrompt FAP xpConfig (const $ return titles) (\title' -> raise (title =? title'))

windowTitles :: X [String]
windowTitles = mapM (runQuery title) =<< windows'

windows' :: X [Window]
windows' = withWindowSet (return . allWindows)
