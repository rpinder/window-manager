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
      code <- keysymToKeycode dpy (stringToKeysym a)
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
  root_attr <- getWindowAttributes dpy $ defaultRootWindow dpy
  attr <- getWindowAttributes dpy win 
  let maxx = (fromIntegral $ wa_width root_attr) - (fromIntegral $ wa_width attr)
      maxy = (fromIntegral $ wa_height root_attr) - (fromIntegral $ wa_height attr)
      newx = f (fromIntegral (wa_x attr))
      newy = g (fromIntegral (wa_y attr))
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
  moveWindow dpy win x y

mapWindowSize :: Display -> Window -> (Dimension -> Dimension) -> (Dimension -> Dimension) -> IO ()
mapWindowSize dpy win f g = do
  root_attr <- getWindowAttributes dpy $ defaultRootWindow dpy
  attr <- getWindowAttributes dpy win
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

  resizeWindow dpy win width height

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
