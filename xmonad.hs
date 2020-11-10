{-# LANGUAGE ExistentialQuantification #-}

import XMonad
import XMonad.Core
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
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
import Data.Default
import System.IO
import System.Directory (doesFileExist, getHomeDirectory)
import System.Process (readProcessWithExitCode, system)
import qualified Data.List as L
import Control.Applicative ((<$>))

setVolume :: String -> X ()
setVolume vol = liftIO $ system ("amixer set Master " ++ vol) >> return ()

getVolume :: X String
getVolume = do
   (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "amixer" ["get", "Master"] ""
   return $ takeWhile (\c -> c /= '[') stdout

main :: IO ()
main = do
   xmrc   <- xmobarrc
   xmproc <- spawnPipe $ "xmobar " ++ xmrc
   xmonad $ ewmh $ fullscreenSupport $ def
      { terminal = "xterm"
      , modMask = myModMask
      , focusedBorderColor = "blue"
      , borderWidth = 4
      , handleEventHook = docksEventHook
      , layoutHook = layoutHook'
      , manageHook = composeAll
         [ -- new windows are always opened in master
           doF SS.swapMaster

           -- open these applications with floating windows
         , ("VisPER_MESA_SQUISH" `L.isPrefixOf`) <$> className --> doFloat
         , ("VisPER_SQUISH" `L.isPrefixOf`) <$> className --> doFloat
         , ("xfig" `L.isPrefixOf`) <$> className --> doFloat
         ]
      }
      `additionalKeys`
         [ ((myModMask .|. shiftMask, xK_s), spawn "sudo /sbin/poweroff")
         , ((myModMask .|. shiftMask, xK_r), spawn "sudo /sbin/reboot")
         , ((myModMask .|. shiftMask, xK_l), spawn "lock")
         , ((myModMask, xK_Escape)         , setVolume "mute")
         , ((myModMask, xK_F1)             , setVolume "unmute 2-")
         , ((myModMask, xK_F2)             , setVolume "unmute 2+")
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
