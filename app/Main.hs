module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Monad
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.Bits 
import Control.Monad.Reader
import Control.Monad.State
import System.Process

data Xstate = Xstate
            { display :: Display
            , root :: Window
            , keybinds :: M.Map (KeyCode, KeyMask) Action
            , focused :: Window
            }

changeFocused :: Window -> Xstate -> Xstate
changeFocused win xs = xs{focused=win}

type X = StateT Xstate IO ()

data Action = MoveLeft
            | MoveUp
            | MoveDown
            | MoveRight
            | IncreaseWidth
            | DecreaseWidth
            | IncreaseHeight
            | DecreaseHeight
            | Raise
            | Launch String
            | None
            deriving (Eq, Ord, Show)

keybindsmap :: M.Map Action (String, KeyMask)
keybindsmap = M.fromList [ (MoveLeft, ("h", mod1Mask))
                      , (MoveDown, ("j", mod1Mask))
                      , (MoveUp, ("k", mod1Mask))
                      , (MoveRight, ("l", mod1Mask))
                      , (DecreaseWidth, ("h", mod1Mask .|. shiftMask))
                      , (IncreaseHeight, ("j", mod1Mask .|. shiftMask))
                      , (DecreaseHeight, ("k", mod1Mask .|. shiftMask))
                      , (IncreaseWidth, ("l", mod1Mask .|. shiftMask))
                      , (Launch "dmenu_run", ("d", mod1Mask))
                      , (Raise, ("r", mod1Mask))
                      ]

keysyms :: Display -> IO (M.Map (KeyCode, KeyMask) Action)
keysyms dpy = do
  let
    f (a, b) = do
      code <- keysymToKeycode dpy $ stringToKeysym a
      return (code, b)
  m <- sequence $ M.map f keybindsmap
  return $ mapconvert m
  where
    mapconvert :: Ord a => M.Map k a -> M.Map a k
    mapconvert = M.fromList . map swap . M.toList

main :: IO ()
main = do
  dpy <- openDisplay ""
  keys <- keysyms dpy
  selectInput dpy (defaultRootWindow dpy) $ substructureRedirectMask .|. substructureNotifyMask
  let f (a, b) = grabKey dpy a b (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  mapM_ f $ M.keys keys
  forever $ do
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- getEvent e
      execStateT (handle ev) $ Xstate dpy (defaultRootWindow dpy) keys none

step :: Num a => a
step = 15

mapWindowPos :: (Position -> Position) -> (Position -> Position) -> X
mapWindowPos f g = do
  dpy <- gets display
  win <- gets focused
  if win == none
    then return ()
    else do
      root_attr <- liftIO $ getWindowAttributes dpy $ defaultRootWindow dpy
      attr <- liftIO $ getWindowAttributes dpy win 
      let maxx = (fromIntegral $ wa_width root_attr) - (fromIntegral $ wa_width attr)
          maxy = (fromIntegral $ wa_height root_attr) - (fromIntegral $ wa_height attr)
          newx = f (fromIntegral $ wa_x attr)
          newy = g (fromIntegral $ wa_y attr)
          x = if newx > maxx
              then maxx
              else if newx < 0
                   then 0
                   else newx
          y = if newy > maxy
              then maxy
              else if newy < 0
                   then 0
                   else newy
      liftIO $ moveWindow dpy win x y

mapWindowSize :: (Dimension -> Dimension) -> (Dimension -> Dimension) -> X
mapWindowSize f g = do
  dpy <- gets display
  win <- gets focused
  if win == none
    then return ()
    else do
      root_attr <- liftIO $ getWindowAttributes dpy $ defaultRootWindow dpy
      attr <- liftIO $ getWindowAttributes dpy win
      let maxwidth = (fromIntegral $ wa_width root_attr) - (fromIntegral $ wa_x attr)
          maxheight = (fromIntegral $ wa_height root_attr) - (fromIntegral $ wa_y attr)
          newwidth = f (fromIntegral $ wa_width attr) 
          newheight = g (fromIntegral $ wa_height attr)
          width = if newwidth > maxwidth
                  then maxwidth
                  else if newwidth < 0
                       then 1
                       else newwidth
          height = if newheight > maxheight
                   then maxheight
                   else if newheight < 0
                        then 1
                        else newheight
      liftIO $ resizeWindow dpy win width height

handle :: Event -> X
handle KeyEvent{ev_event_type = typ, ev_subwindow=subwin, ev_state = evstate, ev_keycode = code}
  | typ == keyPress = do
      keys <- gets keybinds
      modify $ changeFocused subwin
      handleAction (fromMaybe None (M.lookup (code, evstate) keys))

handle ConfigureEvent{} = return ()

handle MapRequestEvent{ev_window = window} = do
  dpy <- gets display
  liftIO $ do
    setWindowBorderWidth dpy window 5
    setWindowBorder dpy window $ blackPixel dpy (defaultScreen dpy)
    mapWindow dpy window

handle ConfigureRequestEvent{ev_x = x, ev_y = y, ev_width = width, ev_height = height, ev_border_width = border_width, ev_above = above, ev_detail = detail, ev_window = window, ev_value_mask = value_mask} = do
  dpy <- gets display
  let wc = WindowChanges x y width height border_width above detail
  liftIO $ configureWindow dpy window value_mask wc

handle x  = liftIO $ print x

handleAction :: Action -> X
handleAction MoveLeft = mapWindowPos (subtract step) id
handleAction MoveDown = mapWindowPos id (+ step)
handleAction MoveUp = mapWindowPos id (subtract step)
handleAction MoveRight = mapWindowPos (+ step) id
handleAction Raise = do
  dpy <- gets display
  win <- gets focused
  if win == none
    then return ()
    else liftIO $ raiseWindow dpy win
handleAction IncreaseWidth = mapWindowSize (+ step) id
handleAction DecreaseWidth = mapWindowSize (subtract step) id
handleAction IncreaseHeight = mapWindowSize id (+ step)
handleAction DecreaseHeight = mapWindowSize id (subtract step)
handleAction (Launch cmd) = liftIO $ do
  _ <- runCommand cmd
  return ()
handleAction x  = liftIO $ print x
