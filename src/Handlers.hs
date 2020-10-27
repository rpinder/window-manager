module Handlers where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M
import System.Process (runCommand)
import Control.Monad.State
import Data.Maybe (fromMaybe)

import Types
import Utils
import Config
import Helpers

handle :: Event -> X ()
handle KeyEvent{ev_event_type = typ, ev_state = evstate, ev_keycode = code}
  | typ == keyPress = do
      keys <- gets keybinds
      handleAction (fromMaybe None (M.lookup (code, evstate) keys))

handle ConfigureEvent{ev_x = x, ev_y = y, ev_width = width, ev_height = height, ev_window = window} = do
  return ()

handle MapRequestEvent{ev_window = window} = do
  dpy <- gets display
  io $ do
    borderWidth <- borderWidth <$> config
    setWindowBorderWidth dpy window $ fi borderWidth
    mapWindow dpy window
  onJust (windowToClient window) $ \c -> setFocus c

handle ConfigureRequestEvent{ev_x = x, ev_y = y, ev_width = width, ev_height = height, ev_border_width = border_width, ev_above = above, ev_detail = detail, ev_window = window, ev_value_mask = value_mask} = do
  dpy <- gets display
  let wc = WindowChanges x y width height border_width above detail
  io $ configureWindow dpy window value_mask wc
  ws <- gets windows
  client <- windowToClient window
  case client of
    Just w -> modify $ \s -> s{windows=(Client (fi x) (fi y) (fi width) (fi height) window) : filter (/= w) ws}
    Nothing -> modify $ \s -> s{windows=(Client (fi x) (fi y) (fi width) (fi height) window) : ws}

handle ButtonEvent{ev_event_type = typ, ev_subwindow = win, ev_button = but}
  | typ == buttonRelease = do
      dpy <- gets display
      io $ ungrabPointer dpy currentTime
      onJust (gets dragging) $ \_ -> do
          modify $ \s -> s{dragging=Nothing}
          onJust (windowToClient win) $ \c -> modify $ \s -> s{focused=Just c}
  | typ == buttonPress = do
      onJust (windowToClient win) $ \c -> do
        setFocus c
        handleAction Raise
        case but of
          1 -> mouseMoveClient c
          3 -> mouseResizeClient c

handle MotionEvent{ev_x = ex, ev_y = ey} = do
  onJust (gets dragging) $ \f -> f (fromIntegral ex) (fromIntegral ey)
    
handle _ = return ()

handleAction :: Action -> X ()
handleAction MoveLeft = mapWindowPos (subtract 15) id
handleAction MoveDown = mapWindowPos id (+ 15)
handleAction MoveUp = mapWindowPos id (subtract 15)
handleAction MoveRight = mapWindowPos (+ 15) id
handleAction Raise = onJust (gets focused) $ \c -> raiseClient c
handleAction IncreaseWidth = mapWindowSize (+ 15) id
handleAction DecreaseWidth = mapWindowSize (subtract 15) id
handleAction IncreaseHeight = mapWindowSize id (+ 15)
handleAction DecreaseHeight = mapWindowSize id (subtract 15)
handleAction (Launch cmd) = io $ do
  _ <- runCommand cmd
  return ()
handleAction Quit = modify $ \s -> s{quit=True}
handleAction _  = return ()

