module Helpers where

import Graphics.X11.Xlib
import Control.Monad.State
import Data.List
import Graphics.X11.Xlib.Extras
import Data.Bits ((.|.))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Types
import Utils
import Config

windowToClient :: Window -> X (Maybe Client)
windowToClient win = do
  ws <- gets workspaces
  cw <- gets current_ws
  return $ find (\c -> c_window c == win) (ws V.! (cw - 1))

focusedClient :: X (Maybe Client)
focusedClient = do
  foc <- gets focused
  case foc of
    Just n -> do
      ws <- gets workspaces
      cw <- gets current_ws
      return $ Just ((ws V.! (cw - 1)) !! n)
    Nothing -> return Nothing

mapClientPos :: (Position -> Position) -> (Position -> Position) -> X ()
mapClientPos f g = do
  onJust focusedClient $ \c -> do
      let x' = f $ c_x c
          y' = g $ c_y c
      moveClient c x' y' True

mapClientSize :: (Dimension -> Dimension) -> (Dimension -> Dimension) -> X ()
mapClientSize f g = do
    onJust focusedClient $ \c -> do
      let width = f $ c_width c
          height = g $ c_height c
      resizeClient c width height True

unFocus :: X ()
unFocus = do
  dpy <- gets display
  bup <- getColor BorderUnfocused
  onJust focusedClient $ \c -> io $ setWindowBorder dpy (c_window c) bup

setFocus :: Client -> X ()
setFocus client = do
    bfp <- getColor BorderFocused
    dpy <- gets display
    io $ do
      setInputFocus dpy (c_window client) revertToParent currentTime
      setWindowBorder dpy (c_window client) bfp
    ws <- gets workspaces
    cw <- gets current_ws
    case findIndex (== client) (ws V.! (cw - 1)) of
      Just i -> modify $ \s -> s{focused=Just i}
      Nothing -> return ()

raiseClient :: Client -> X ()
raiseClient client = do
  dpy <- gets display
  liftIO $ raiseWindow dpy $ c_window client

maximizeClient :: Client -> X ()
maximizeClient client = do
  dpy <- gets display
  moveClient client 0 0 False
  attr <- io $ getWindowAttributes dpy $ defaultRootWindow dpy
  let width = fi $ wa_width attr
  let height = fi $ wa_height attr
  resizeClient client width height True


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

moveClient :: Client -> Position -> Position -> Bool -> X ()
moveClient c@Client{c_width=width, c_height=height, c_window=win} x y b = do
  dpy <- gets display
  ws <- gets workspaces
  cw <- gets current_ws
  modify $ \s -> s{workspaces = ws V.// [(cw-1, (Client x y width height win) : filter (\c -> c_window c /= win) (ws V.! (cw - 1)))]}
  io $ moveWindow dpy (c_window c) x y
  when b $ setFocus $ ws V.! (cw - 1) !! 0

resizeClient :: Client -> Dimension -> Dimension -> Bool -> X ()
resizeClient c@Client{c_x=x,c_y=y, c_window=win} w h b = do
  dpy <- gets display
  ws <- gets workspaces
  cw <- gets current_ws
  modify $ \s -> s{workspaces = ws V.// [(cw-1, (Client x y w h win) : filter (\c -> c_window c /= win) (ws V.! (cw - 1)))]}
  io $ resizeWindow dpy win w h
  when b $ setFocus $ ws V.! (cw - 1) !! 0

mouseMoveClient :: Client -> X ()
mouseMoveClient client = do
  let win = c_window client
  dpy <- gets display
  (_, _, _, ox, oy, _, _, _) <- io $ queryPointer dpy win
  wa <- io $ getWindowAttributes dpy win
  mouseDrag (\ex ey -> moveClient client (wa_x wa .+ ex .- fi ox) (wa_y wa .+ ey .- fi oy) True)

mouseResizeClient :: Client -> X ()
mouseResizeClient client = do
  let win = c_window client
  dpy <- gets display
  (_, _, _, ox, oy, _, _, _) <- io $ queryPointer dpy win
  wa <- io $ getWindowAttributes dpy win
  mouseDrag (\ex ey -> resizeClient client (wa_width wa .+ ex .- ox) (wa_height wa .+ ey .- oy) True)

-- allocColors :: Display -> [Config -> String] -> IO (M.Map String Pixel)
-- allocColors dpy fs = mconcat <$> res
--   where res = forM fs $ \f -> do
--           pix <- initColor dpy . f =<< config
--           cfg <- config
--           return $ M.singleton (f cfg) pix

getColor :: ColorOption -> X Pixel
getColor co = do
  colors <- gets colors
  return $ fromMaybe 0 $ M.lookup co colors

onJust :: (X (Maybe a)) -> (a -> X ()) -> X ()
onJust x f = do
  res <- x
  case res of
    Nothing -> return ()
    Just a -> f a

unmapClients :: X ()
unmapClients = do
  ws <- gets workspaces
  cw <- gets current_ws
  dpy <- gets display
  io $ forM_ (ws V.! (cw - 1)) $ \c -> do
    unmapWindow dpy $ c_window c
    
switchToWorkspace :: Int -> X ()
switchToWorkspace x = do
  bup <- getColor BorderUnfocused
  dpy <- gets display
  onJust focusedClient $ \c -> io $ setWindowBorder dpy (c_window c) bup
  ws <- gets workspaces
  unmapClients
  dpy <- gets display
  rt <- gets root
  io $ do
    ewmhSetCurrentDesktop dpy rt x
    forM_ (ws V.! (x-1)) $ \w -> do
      mapWindow dpy $ c_window w
  modify $ \s -> s{current_ws=x}
  case listToMaybe (ws V.! (x-1)) of
    Just win -> setFocus win
    Nothing -> modify $ \s -> s{focused=Nothing}

ewmhSetCurrentDesktop :: Display -> Window -> Int -> IO ()
ewmhSetCurrentDesktop dpy rt x = do
    current_desktop_atom <- internAtom dpy "_NET_CURRENT_DESKTOP" False
    cardinal <- internAtom dpy "CARDINAL" False
    changeProperty32 dpy rt current_desktop_atom cardinal propModeReplace [fromIntegral x]

closeClient :: Client -> X ()
closeClient c@Client{c_window=win} = do
  dpy <- gets display
  io $ allocaXEvent $ \ev -> do
    protocols <- internAtom dpy "WM_PROTOCOLS" False
    delete <- internAtom dpy "WM_DELETE_WINDOW" False
    setEventType ev clientMessage
    setClientMessageEvent ev win protocols 32 delete 0
    sendEvent dpy win False noEventMask ev
  ws_list <- gets workspaces
  cw <- gets current_ws
  let ws' = filter (/= c) (ws_list V.! (cw - 1))
  modify $ \s -> s{workspaces=ws_list V.// [(cw-1, ws')] }
  case listToMaybe ws' of
    Just c -> setFocus c
    Nothing -> modify $ \s -> s{focused=Nothing}

settingsToColormap :: Display -> M.Map COptions ResultValue -> IO (M.Map ColorOption Pixel)
settingsToColormap dpy = M.foldrWithKey f (pure M.empty)
  where f (CColor co) (Colour str) b = do
          pix <- initColor dpy (T.unpack str)
          b' <- b
          return $ M.insert co pix b'
        f _ _ b = b
