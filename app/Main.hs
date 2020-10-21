module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Maybe
import Data.Bits 
import Control.Monad.State
import System.Process
import Data.List

data Xstate = Xstate
            { display :: Display
            , root :: Window
            , keybinds :: M.Map (KeyCode, KeyMask) Action
            , focused :: Maybe Client
            , dragging :: !(Maybe (Position -> Position -> X ()))
            , windows :: ![Client]
            , quit :: Bool
            }

data Client = Client
            { c_x :: Position 
            , c_y :: Position
            , c_width :: Dimension
            , c_height :: Dimension
            , c_window :: Window
            }
  deriving (Eq, Show)

type X a = StateT Xstate IO a

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
            | Quit
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
                      , (Quit, ("q", mod1Mask .|. shiftMask))
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
  grabButton dpy 1 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none
  grabButton dpy 3 mod1Mask (defaultRootWindow dpy) True (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none
  forM_ (M.keys keys) $ \(a, b) -> 
    grabKey dpy a b (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  allocaXEvent $ \e -> loop e $ Xstate dpy (defaultRootWindow dpy) keys Nothing Nothing [] False

loop :: XEventPtr -> Xstate -> IO ()
loop e s@Xstate{display=dpy}= do
    nextEvent dpy e
    ev <- getEvent e
    s' <- execStateT (handle ev) s
    unless (quit s') (loop e s')

step :: Num a => a
step = 15

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

io :: MonadIO m => IO a -> m a
io = liftIO

windowToClient :: Window -> X (Maybe Client)
windowToClient win = do
  ws <- gets windows
  return $ find (\c -> c_window c == win) ws

mapWindowPos :: (Position -> Position) -> (Position -> Position) -> X ()
mapWindowPos f g = do
  dpy <- gets display
  client <- gets focused
  case client of
    Just c -> do
      let x' = f $ c_x c
          y' = g $ c_y c
      io $ moveWindow dpy (c_window c) x' y' 
    Nothing -> return ()

mapWindowSize :: (Dimension -> Dimension) -> (Dimension -> Dimension) -> X ()
mapWindowSize f g = do
  dpy <- gets display
  client <- gets focused
  case client of
    Just c -> do
      let width = f $ c_width c
          height = g $ c_height c
      io $ resizeWindow dpy (c_window c) width height
    Nothing -> return ()

setFocus :: Client -> X ()
setFocus client = do
    dpy <- gets display
    io $ setInputFocus dpy (c_window client) revertToParent currentTime
    modify $ \s -> s{focused=Just client}

handle :: Event -> X ()
handle KeyEvent{ev_event_type = typ, ev_state = evstate, ev_keycode = code}
  | typ == keyPress = do
      keys <- gets keybinds
      handleAction (fromMaybe None (M.lookup (code, evstate) keys))

handle ConfigureEvent{ev_x = x, ev_y = y, ev_width = width, ev_height = height, ev_window = window} = do
  ws <- gets windows
  client <- windowToClient window
  case client of
    Just w -> modify $ \s -> s{windows=(Client (fi x) (fi y) (fi width) (fi height) window) : filter (/= w) ws}
    Nothing -> modify $ \s -> s{windows=(Client (fi x) (fi y) (fi width) (fi height) window) : ws}

handle MapRequestEvent{ev_window = window} = do
  dpy <- gets display
  io $ do
    setWindowBorderWidth dpy window 5
    setWindowBorder dpy window $ blackPixel dpy (defaultScreen dpy)
    mapWindow dpy window
  client <- windowToClient window
  case client of
    Just c -> setFocus c
    Nothing -> return ()

handle ConfigureRequestEvent{ev_x = x, ev_y = y, ev_width = width, ev_height = height, ev_border_width = border_width, ev_above = above, ev_detail = detail, ev_window = window, ev_value_mask = value_mask} = do
  dpy <- gets display
  let wc = WindowChanges x y width height border_width above detail
  io $ configureWindow dpy window value_mask wc

handle ButtonEvent{ev_event_type = typ, ev_subwindow = win, ev_x = x, ev_y = y, ev_button = but, ev_root = root}
  | typ == buttonRelease = do
      drag <- gets dragging
      dpy <- gets display
      io $ ungrabPointer dpy currentTime
      case drag of
        Just _ -> modify $ \s -> s{dragging=Nothing}
        Nothing -> return ()
  | typ == buttonPress = do
      client <- windowToClient win
      case client of
        Just c -> do
          setFocus c
          handleAction Raise
          case but of
            1 -> mouseMoveClient c
            3 -> mouseResizeClient c
        Nothing -> return ()

handle MotionEvent{ev_x = ex, ev_y = ey} = do
  drag <- gets dragging
  case drag of
    Just f -> f (fromIntegral ex) (fromIntegral ey)
    Nothing -> return ()
    
handle _ = return ()

raiseClient :: Client -> X ()
raiseClient client = do
  dpy <- gets display
  liftIO $ raiseWindow dpy $ c_window client

handleAction :: Action -> X ()
handleAction MoveLeft = mapWindowPos (subtract step) id
handleAction MoveDown = mapWindowPos id (+ step)
handleAction MoveUp = mapWindowPos id (subtract step)
handleAction MoveRight = mapWindowPos (+ step) id
handleAction Raise = do
  client <- gets focused
  case client of
    Just c -> raiseClient c
    Nothing -> return ()
handleAction IncreaseWidth = mapWindowSize (+ step) id
handleAction DecreaseWidth = mapWindowSize (subtract step) id
handleAction IncreaseHeight = mapWindowSize id (+ step)
handleAction DecreaseHeight = mapWindowSize id (subtract step)
handleAction (Launch cmd) = io $ do
  _ <- runCommand cmd
  return ()
handleAction Quit = modify $ \s -> s{quit=True}
handleAction _  = return ()

clearEventsOfMask :: EventMask -> X ()
clearEventsOfMask mask = do
  dpy <- gets display
  io $ do
    sync dpy False
    allocaXEvent $ \e -> fix $
      \f -> checkMaskEvent dpy mask e >>= flip when f

mouseDrag :: (Position -> Position -> X ()) -> X ()
mouseDrag f = do
  drag <- gets dragging
  case drag of
    Just _ -> return ()
    Nothing -> do
      dpy <- gets display
      root <- gets root
      io $ grabPointer dpy root False (buttonReleaseMask .|. pointerMotionMask) grabModeAsync grabModeAsync none none currentTime
      modify $ \s -> s{dragging = Just (\x y -> clearEventsOfMask pointerMotionMask >> f x y)}

mouseMoveClient :: Client -> X ()
mouseMoveClient client = do
  let win = c_window client
  dpy <- gets display
  (_, _, _, ox, oy, _, _, _) <- io $ queryPointer dpy win
  wa <- io $ getWindowAttributes dpy win
  mouseDrag (\ex ey -> io $ moveWindow dpy win (wa_x wa .+ ex .- fi ox) (wa_y wa .+ ey .- fi oy))

(.+) :: (Integral a, Integral b, Num c) => a -> b -> c
a .+ b = fi a + fi b

(.-) :: (Integral a, Integral b, Num c) => a -> b -> c
a .- b = fi a - fi b
  
mouseResizeClient :: Client -> X ()
mouseResizeClient client = do
  let win = c_window client
  dpy <- gets display
  (_, _, _, ox, oy, _, _, _) <- io $ queryPointer dpy win
  wa <- io $ getWindowAttributes dpy win
  mouseDrag (\ex ey -> io $ resizeWindow dpy win (wa_width wa .+ ex .- ox) (wa_height wa .+ ey .- oy))

