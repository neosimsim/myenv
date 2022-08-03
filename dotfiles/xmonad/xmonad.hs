{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Weverything -Werror #-}
{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

import Graphics.X11.ExtraTypes
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioRaiseVolume,
    xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
    xK_Arabic_sheen,
  )
import XMonad
  ( Default (def),
    Full (Full),
    KeyMask,
    KeySym,
    Resize (Expand, Shrink),
    Tall (Tall),
    Window,
    X,
    XConfig
      ( XConfig,
        borderWidth,
        focusFollowsMouse,
        focusedBorderColor,
        layoutHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal
      ),
    className,
    composeAll,
    doFloat,
    doShift,
    mod4Mask,
    runQuery,
    sendMessage,
    shiftMask,
    spawn,
    stringProperty,
    title,
    windows,
    withWindowSet,
    xK_BackSpace,
    xK_Down,
    xK_Left,
    xK_Print,
    xK_Right,
    xK_Up,
    xK_a,
    xK_b,
    xK_g,
    xK_l,
    xK_p,
    xK_q,
    xK_w,
    xmonad,
    (-->),
    (.|.),
    (=?),
    (|||),
  )
import XMonad.Actions.WindowGo (raise)
import XMonad.Hooks.DynamicLog
  ( PP (ppCurrent, ppTitle, ppUrgent),
    shorten,
    statusBar,
    wrap,
    xmobarPP,
  )
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.Rescreen (addRandrChangeHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook (NoUrgencyHook), focusUrgent, withUrgencyHook)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
  ( XPConfig
      ( autoComplete,
        height,
        maxComplColumns,
        searchPredicate,
        sorter
      ),
    XPrompt (showXPrompt),
    mkComplFunFromList',
    mkXPrompt,
  )
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.StackSet qualified as W
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
    ((myModMask, xK_w), focusApplicationPrompt)
  ]

myStartupHook = do
  startupHook def
  setWMName "LG3D"

data FocusApplicationPrompt = FAP

instance XPrompt FocusApplicationPrompt where
  showXPrompt FAP = "Focus Window: "

focusApplicationPrompt :: X ()
focusApplicationPrompt = do
  titles <- mapM (runQuery title) =<< windows'
  let xpConfig =
        def
          { height = 30,
            maxComplColumns = Just 1,
            searchPredicate = fuzzyMatch,
            sorter = fuzzySort,
            autoComplete = Just 2
          }

  mkXPrompt FAP xpConfig (mkComplFunFromList' xpConfig titles) (\title' -> raise (title =? title'))

windows' :: X [Window]
windows' = withWindowSet (return . W.allWindows)
