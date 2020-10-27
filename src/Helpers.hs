module Helpers where

import Graphics.X11.Xlib
import Control.Monad.State
import Data.List
import Graphics.X11.Xlib.Extras
import Data.Bits ((.|.))
import Data.Maybe
import qualified Data.Map as M

import Types
import Utils
import Config

windowToClient :: Window -> X (Maybe Client)
windowToClient win = do
  ws <- gets windows
  return $ find (\c -> c_window c == win) ws

mapWindowPos :: (Position -> Position) -> (Position -> Position) -> X ()
mapWindowPos f g = do
  onJust (gets focused) $ \c -> do
      let x' = f $ c_x c
          y' = g $ c_y c
      dpy <- gets display
      io $ moveWindow dpy (c_window c) x' y' 
      modify $ \s -> s{focused=Just c{c_x=x', c_y=y'}}

mapWindowSize :: (Dimension -> Dimension) -> (Dimension -> Dimension) -> X ()
mapWindowSize f g = do
    onJust (gets focused) $ \c -> do
      let width = f $ c_width c
          height = g $ c_height c
      dpy <- gets display
      io $ resizeWindow dpy (c_window c) width height
      modify $ \s -> s{focused=Just c{c_width=width, c_height=height}}

setFocus :: Client -> X ()
setFocus client = do
    bfp <- getColor borderFocusedColor
    bup <- getColor borderUnfocusedColor
    dpy <- gets display
    onJust (gets focused) $ \c -> io $ setWindowBorder dpy (c_window c) bup
    io $ do
      setInputFocus dpy (c_window client) revertToParent currentTime
      setWindowBorder dpy (c_window client) bfp
    modify $ \s -> s{focused=Just client}

raiseClient :: Client -> X ()
raiseClient client = do
  dpy <- gets display
  liftIO $ raiseWindow dpy $ c_window client

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

mouseResizeClient :: Client -> X ()
mouseResizeClient client = do
  let win = c_window client
  dpy <- gets display
  (_, _, _, ox, oy, _, _, _) <- io $ queryPointer dpy win
  wa <- io $ getWindowAttributes dpy win
  mouseDrag (\ex ey -> io $ resizeWindow dpy win (wa_width wa .+ ex .- ox) (wa_height wa .+ ey .- oy))

allocColors :: Display -> [Config -> String] -> IO (M.Map String Pixel)
allocColors dpy fs = mconcat <$> res
  where res = forM fs $ \f -> do
          pix <- initColor dpy . f =<< config
          cfg <- config
          return $ M.singleton (f cfg) pix

getColor :: (Config -> String) -> X Pixel
getColor f = do
  colors <- gets colors
  res <- io $ f <$> config
  return $ fromMaybe 0 $ M.lookup res colors

onJust :: (X (Maybe a)) -> (a -> X ()) -> X ()
onJust x f = do
  res <- x
  case res of
    Nothing -> return ()
    Just a -> f a
