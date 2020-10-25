module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M
import Data.Bits 
import Control.Monad.State

import Types
import Handlers
import Config

main :: IO ()
main = do
  dpy <- openDisplay ""
  keys <- keysyms dpy
  selectInput dpy (defaultRootWindow dpy) $ substructureRedirectMask .|. substructureNotifyMask
  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none
  forM_ (M.keys keys) $ \(a, b) -> 
    grabKey dpy a b (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  fcolor <- initColor dpy . borderFocusedColor =<< config
  ufcolor <- initColor dpy . borderUnfocusedColor =<< config
  allocaXEvent $ \e -> loop e $ Xstate dpy (defaultRootWindow dpy) keys Nothing Nothing [] False fcolor ufcolor

loop :: XEventPtr -> Xstate -> IO ()
loop e s@Xstate{display=dpy}= do
    nextEvent dpy e
    ev <- getEvent e
    s' <- execStateT (handle ev) s
    unless (quit s') (loop e s')
