module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M
import Data.Bits 
import Control.Monad.State
import qualified Data.Vector as V

import Types
import Handlers
import Helpers
import Config

main :: IO ()
main = do
  dpy <- openDisplay ""
  selectInput dpy (defaultRootWindow dpy) $ substructureRedirectMask .|. substructureNotifyMask

  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none

  cfg <- readConfig "configrc"
  cmap <- settingsToColormap dpy $ _settings cfg
  keybindings <- stringToKeyCode dpy $ _keybinds cfg

  forM_ (M.keys keybindings) $ \(a, b) -> 
    grabKey dpy a b (defaultRootWindow dpy) True grabModeAsync grabModeAsync

  let root = defaultRootWindow dpy

  ewmhSetCurrentDesktop dpy root 1

  allocaXEvent $ \e -> loop e $ Xstate dpy root keybindings (_settings cfg) Nothing Nothing (V.fromList . take 10 $ repeat []) 1 False cmap

loop :: XEventPtr -> Xstate -> IO ()
loop e s@Xstate{display=dpy}= do
    nextEvent dpy e
    ev <- getEvent e
    s' <- execStateT (handle ev) s
    unless (quit s') (loop e s')


 
