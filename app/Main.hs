module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Monad
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.Char (isUpper)
import Data.Bits 

data Action = MoveLeft
            | MoveUp
            | MoveDown
            | MoveRight
            | IncreaseWidth
            | DecreaseWidth
            | IncreaseHeight
            | DecreaseHeight
            | Raise
            | None
            deriving (Eq, Ord)

keybinds :: M.Map Action (String, KeyMask)
keybinds = M.fromList [ (MoveLeft, ("h", mod1Mask))
                      , (MoveDown, ("j", mod1Mask))
                      , (MoveUp, ("k", mod1Mask))
                      , (MoveRight, ("l", mod1Mask))
                      , (DecreaseWidth, ("h", mod1Mask .|. shiftMask))
                      , (IncreaseHeight, ("j", mod1Mask .|. shiftMask))
                      , (DecreaseHeight, ("k", mod1Mask .|. shiftMask))
                      , (IncreaseWidth, ("l", mod1Mask .|. shiftMask))
                      , (Raise, ("r", mod1Mask))
                      ]

keysyms :: Display -> IO (M.Map (KeyCode, KeyMask) Action)
keysyms dpy = do
  let
    f (a, b) = do
      code <- (keysymToKeycode dpy (stringToKeysym a))
      return (code, b)
  m <- sequence $ M.map f keybinds
  return $ mapconvert m
  where
    mapconvert :: Ord a => M.Map k a -> M.Map a k
    mapconvert = M.fromList . map swap . M.toList

main :: IO ()
main = do
  dpy <- openDisplay ""
  keys <- keysyms dpy
  let
    f (a, b) = grabKey dpy a b (defaultRootWindow dpy) True grabModeAsync grabModeAsync
  mapM_ f (M.keys keys)
  forever $ do
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- getEvent e
      handle dpy ev

step :: Num a => a
step = 15

mapWindowPos :: Display -> Window -> (Position -> Position) -> (Position -> Position) -> IO ()
mapWindowPos dpy win f g = do
  attr <- getWindowAttributes dpy win 
  moveWindow dpy win (f (fromIntegral (wa_x attr))) (g (fromIntegral (wa_y attr)))

mapWindowSize :: Display -> Window -> (Dimension -> Dimension) -> (Dimension -> Dimension) -> IO ()
mapWindowSize dpy win f g = do
  attr <- getWindowAttributes dpy win
  resizeWindow dpy win (f (fromIntegral (wa_width attr))) (g (fromIntegral (wa_height attr)))

handle :: Display -> Event -> IO ()
handle dpy (KeyEvent{ev_event_type = typ, ev_subwindow=subwin, ev_state = state, ev_keycode = code})
  | typ == keyPress = do
      keys <- keysyms dpy
      handleAction (fromMaybe None (M.lookup (code, state) keys)) dpy subwin
handle _ _  = return () 

handleAction :: Action -> Display -> Window -> IO ()
handleAction MoveLeft dpy win = mapWindowPos dpy win (subtract step) id
handleAction MoveDown dpy win = mapWindowPos dpy win id (+ step)
handleAction MoveUp dpy win = mapWindowPos dpy win id (subtract step)
handleAction MoveRight dpy win = mapWindowPos dpy win (+ step) id
handleAction Raise dpy win = raiseWindow dpy win
handleAction IncreaseWidth dpy win = mapWindowSize dpy win (+ step) id
handleAction DecreaseWidth dpy win = mapWindowSize dpy win (subtract step) id
handleAction IncreaseHeight dpy win = mapWindowSize dpy win id (+ step)
handleAction DecreaseHeight dpy win = mapWindowSize dpy win id (subtract step)
handleAction _ _ _ = return ()
