module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Monad

main :: IO ()
main = do
  dpy <- openDisplay ""
  f1 <- keysymToKeycode dpy (stringToKeysym "F1")
  h <- keysymToKeycode dpy (stringToKeysym "h")
  j <- keysymToKeycode dpy (stringToKeysym "j")
  k <- keysymToKeycode dpy (stringToKeysym "k")
  l <- keysymToKeycode dpy (stringToKeysym "l")
  grabKey dpy f1 mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  grabKey dpy h mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  grabKey dpy k mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  grabKey dpy j mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  grabKey dpy l mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  forever $ do
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- getEvent e
      handle dpy ev f1 h j k l

step :: Num a => a
step = 15

handle :: Display -> Event -> KeyCode -> KeyCode -> KeyCode -> KeyCode -> KeyCode -> IO ()
handle dpy (KeyEvent{ev_event_type = typ, ev_subwindow=subwin, ev_keycode = code}) f1 h j k l
  | typ == keyPress = do
    handleKeyPress code
    where
      handleKeyPress :: KeyCode -> IO ()
      handleKeyPress keycode
        | keycode == f1 = raiseWindow dpy subwin
        | keycode == h = do
            attr <- getWindowAttributes dpy subwin
            moveWindow dpy subwin (fromIntegral (wa_x attr) - step) (fromIntegral (wa_y attr))
        | keycode == j = do
            attr <- getWindowAttributes dpy subwin
            moveWindow dpy subwin (fromIntegral (wa_x attr)) (fromIntegral (wa_y attr) + step)
        | keycode == k = do
            attr <- getWindowAttributes dpy subwin
            moveWindow dpy subwin (fromIntegral (wa_x attr)) (fromIntegral (wa_y attr) - step)
        | keycode == l = do
            attr <- getWindowAttributes dpy subwin
            moveWindow dpy subwin (fromIntegral (wa_x attr) + step) (fromIntegral (wa_y attr))
        | otherwise = return ()
handle _ _ _ _ _ _ _ = return () 
