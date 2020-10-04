module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Monad
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)

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

keybinds :: M.Map Action String
keybinds = M.fromList [ (MoveLeft, "h")
                      , (MoveDown, "j")
                      , (MoveUp, "k")
                      , (MoveRight, "l")
                      , (Raise, "r")
                      ]

keysyms :: Display -> IO (M.Map KeyCode Action)
keysyms dpy = do
  m <- sequence $ M.map (\x -> keysymToKeycode dpy (stringToKeysym x)) keybinds
  return $ mapconvert m
  where
    mapconvert :: Ord a => M.Map k a -> M.Map a k
    mapconvert = M.fromList . map swap . M.toList

main :: IO ()
main = do
  dpy <- openDisplay ""
  keys <- keysyms dpy
  mapM_ (\x -> grabKey dpy x mod1Mask (defaultRootWindow dpy) True grabModeAsync grabModeAsync) (M.keys keys)
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

handle :: Display -> Event -> IO ()
handle dpy (KeyEvent{ev_event_type = typ, ev_subwindow=subwin, ev_keycode = code})
  | typ == keyPress = do
      keys <- keysyms dpy
      handleAction (fromMaybe None (M.lookup code keys)) dpy subwin
handle _ _  = return () 

handleAction :: Action -> Display -> Window -> IO ()
handleAction MoveLeft dpy win = mapWindowPos dpy win (subtract step) id
handleAction MoveDown dpy win = mapWindowPos dpy win id (+ step)
handleAction MoveUp dpy win = mapWindowPos dpy win id (subtract step)
handleAction MoveRight dpy win = mapWindowPos dpy win (+ step) id
handleAction Raise dpy win = raiseWindow dpy win
handleAction _ _ _ = return ()
