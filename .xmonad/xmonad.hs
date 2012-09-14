import XMonad
import Data.Map    (fromList)
import Data.Monoid (mappend)
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Util.Dzen
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import qualified XMonad.StackSet as W

alert = dzenConfig centered . show . round
centered =
        onCurr (center 150 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ ewmh defaultConfig
      { manageHook = mManageHook
      , layoutHook = avoidStruts  $  layoutHook defaultConfig
      , terminal = "gnome-terminal --hide-menubar"
      , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
      } `additionalKeys`
        ([ ((mod1Mask .|. controlMask, xK_z), spawn "gnome-screensaver-command --lock"),
           ((0, xK_F7), lowerVolume 4 >> return ()),
           ((0, xK_F8), raiseVolume 4 >> return ())
        ]
        ++

        --
        -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
        -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
        --
        [((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_equal, xK_minus] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]])




-- Window rules aka Manage Hook {{{
mManageHook :: ManageHook
mManageHook = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) <+>
               (composeAll . concat $
    [ [ className =? c --> doFloat        | c <- floats ],
      [ className =? w --> doShift "web"  | w <- webs]   ,
      [ className =? g --> doShift "game" | g <- games]  ,
      [ className =? m --> doShift "mail" | m <- mails]  ,
      [ resource  =? "desktop_window"  --> doIgnore
      , className =? "Gimp"            --> doShift "art"
      , title     =? "Save a Bookmark" --> doCenterFloat
      , isFullscreen                   --> doFullFloat
      , isDialog                       --> doCenterFloat
      ] ])
        <+> manageDocks -- make some space
            where floats = ["Mplayer"]
                  webs   = ["Firefox","Google Chrome"]
                  games  = []
                  mails  = ["Thunderbird-bin"]
-- }}}



