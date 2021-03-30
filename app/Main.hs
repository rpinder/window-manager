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
  keys <- keysyms dpy
  selectInput dpy (defaultRootWindow dpy) $ substructureRedirectMask .|. substructureNotifyMask

  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none

  forM_ (M.keys keys) $ \(a, b) -> 
    grabKey dpy a b (defaultRootWindow dpy) True grabModeAsync grabModeAsync

  colors <- allocColors dpy [borderFocusedColor, borderUnfocusedColor]

  let root = defaultRootWindow dpy

  ewmhSetCurrentDesktop dpy root 0

  allocaXEvent $ \e -> loop e $ Xstate dpy root keys Nothing Nothing (V.fromList . take 10 $ repeat []) 0 False colors

loop :: XEventPtr -> Xstate -> IO ()
loop e s@Xstate{display=dpy}= do
    nextEvent dpy e
    ev <- getEvent e
    s' <- execStateT (handle ev) s
    unless (quit s') (loop e s')


 
