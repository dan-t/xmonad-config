{-# LANGUAGE ExistentialQuantification #-}

import XMonad
import XMonad.Core
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
import XMonad.Actions.Volume
import XMonad.Util.Dzen
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.NoBorders
import XMonad.Layout.DragPane
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Fullscreen
import qualified XMonad.StackSet as SS
import XMonad.Config
import System.IO
import System.Directory (doesFileExist, getHomeDirectory)
import qualified Data.List as L
import Control.Applicative ((<$>))


mute = setVolume 0

showVolume = do
   vol  <- getVolume
   let str = if vol == 0 then "[muted]" else show $ round vol
   dzenConfig centered str
   where
      centered =
	 onCurr (center 200 66)
            >=> font "-*-courier new-*-r-*-*-32-*-*-*-*-*-*-*"
            >=> addArgs ["-fg", "#80c0ff"]
            >=> addArgs ["-bg", "#000040"]

main = do
   xmrc   <- xmobarrc
   xmproc <- spawnPipe $ "xmobar " ++ xmrc
   xmonad $ ewmh $ fullscreenSupport $ defaultConfig
      { terminal = "xterm"
      , modMask = myModMask
      , focusedBorderColor = "blue"
      , borderWidth = 4
      , handleEventHook = docksEventHook
      , layoutHook = layoutHook'
      , manageHook = composeAll
         [ -- new windows are always opened in master
           doF SS.swapMaster

           -- open visper squish window floating
         , ("VisPER_MESA_SQUISH" `L.isPrefixOf`) <$> className --> doFloat

           -- open visper squish window floating
         , ("VisPER_SQUISH" `L.isPrefixOf`) <$> className --> doFloat
         ]
      }
      `additionalKeys`
         [ ((myModMask .|. shiftMask, xK_s), spawn "sudo /sbin/poweroff")
         , ((myModMask .|. shiftMask, xK_r), spawn "sudo /sbin/reboot")
         , ((myModMask .|. shiftMask, xK_h), spawn "xflock4" >> spawn "sudo /usr/sbin/pm-hibernate")
         , ((myModMask .|. shiftMask, xK_l), spawn "xflock4")
         , ((myModMask, xK_Escape)         , mute          >> showVolume)
         , ((myModMask, xK_F1)             , lowerVolume 4 >> showVolume)
         , ((myModMask, xK_F2)             , raiseVolume 4 >> showVolume)
         ]
   where
      -- use the windows key as the mod key
      myModMask = mod4Mask

      layoutHook' = avoidStruts (tiled ||| noBorders Full)
         where
            -- default tiling algorithm partitions the screen into two panes
            tiled   = Tall nmaster delta ratio

            -- The default number of windows in the master pane
            nmaster = 1

            -- Default proportion of screen occupied by master pane
            ratio   = 2/3

            -- Percent of screen to increment by when resizing panes
            delta   = 3/100

xmobarrc :: IO FilePath
xmobarrc = do
   hostname <- filter (/= '\n') <$> runProcessWithInput "hostname" [] []
   homedir  <- getHomeDirectory
   let rc     = homedir ++ "/.xmobarrc"
   let hostRc = rc ++ "_" ++ hostname
   hasFile  <- doesFileExist hostRc
   return $ if hasFile then hostRc else rc
