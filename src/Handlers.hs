module Handlers where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M
import qualified Data.Vector as V
import System.Process (runCommand)
import Control.Monad.State
import Control.Monad
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
  io $ mapWindow dpy window
  onJust (windowToClient window) $ \c -> setFocus c

handle ConfigureRequestEvent{ev_x = x, ev_y = y, ev_width = width, ev_height = height, ev_border_width = border_width, ev_above = above, ev_detail = detail, ev_window = window, ev_value_mask = value_mask} = do
  dpy <- gets display
  sets <- gets settings
  let wc = WindowChanges x y width height border_width above detail
  io $ configureWindow dpy window value_mask wc
  res <- isdock dpy
  unless res $ do
    io $ do
      let Just (Number borderWidth) = M.lookup BorderWidth sets
      setWindowBorderWidth dpy window $ fi borderWidth
    ws <- gets workspaces
    cw <- gets current_ws
    client <- windowToClient window
    case client of
      Just w -> modify $ \s -> s{workspaces= ws V.// [(cw, (Client (fi x) (fi y) (fi width) (fi height) window) : filter (/= w) (ws V.! cw))]}
      Nothing -> modify $ \s -> s{workspaces= ws V.// [(cw, (Client (fi x) (fi y) (fi width) (fi height) window) : (ws V.! cw))]}
  where
    isdock dpy = io $ do
      net_wm_window_type <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
      net_wm_window_type_dock <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
      res <- getWindowProperty32 dpy net_wm_window_type window
      case res of
        Nothing -> return False
        Just v -> return $ v!!0 == fromIntegral net_wm_window_type_dock


handle ButtonEvent{ev_event_type = typ, ev_subwindow = win, ev_button = but}
  | typ == buttonRelease = do
      dpy <- gets display
      io $ ungrabPointer dpy currentTime
      onJust (gets dragging) $ \_ -> do
          modify $ \s -> s{dragging=Nothing}
          onJust (windowToClient win) setFocus
  | typ == buttonPress = do
      onJust (windowToClient win) $ \c -> do
        case but of
          1 -> mouseMoveClient c
          3 -> mouseResizeClient c
        handleAction Raise

handle MotionEvent{ev_x = ex, ev_y = ey} = onJust (gets dragging) $ \f -> f (fromIntegral ex) (fromIntegral ey)
    
handle _ = return ()

handleAction :: Action -> X ()
handleAction MoveLeft = mapClientPos (subtract 15) id
handleAction MoveDown = mapClientPos id (+ 15)
handleAction MoveUp = mapClientPos id (subtract 15)
handleAction MoveRight = mapClientPos (+ 15) id
handleAction Raise = onJust focusedClient raiseClient
handleAction IncreaseWidth = mapClientSize (+ 15) id
handleAction DecreaseWidth = mapClientSize (subtract 15) id
handleAction IncreaseHeight = mapClientSize id (+ 15)
handleAction DecreaseHeight = mapClientSize id (subtract 15)
handleAction (Launch cmd) = io $ do
  _ <- runCommand cmd
  return ()
handleAction Quit = modify $ \s -> s{quit=True}
handleAction CloseWindow = onJust focusedClient closeClient
handleAction (SwitchToWorkspace x) = switchToWorkspace x
handleAction _  = return ()

