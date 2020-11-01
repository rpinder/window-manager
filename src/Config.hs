module Config where

import Data.Tuple
import qualified Data.Map as M
import Graphics.X11.Xlib
import Data.Bits
import Numeric
import Data.Word

import Types
import Utils

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
                      , (CloseWindow, ("c", mod1Mask))
                      , (Quit, ("q", mod1Mask .|. shiftMask))
                      ]

keysyms :: Display -> IO (M.Map (KeyCode, KeyMask) Action)
keysyms dpy = do
  let f (a, b) = do
        code <- keysymToKeycode dpy $ stringToKeysym a
        return (code, b)
  m <- sequence $ M.map f keybindsmap
  return $ mapconvert m
  where
    mapconvert :: Ord a => M.Map k a -> M.Map a k
    mapconvert = M.fromList . map swap . M.toList

step :: Num a => a
step = 15

config :: IO Config
config = return $ Config 5 "efdfbb" "2f2c25" 15

stringToColor :: String -> (Word16, Word16, Word16)
stringToColor = tuple . map (*257) . map (fst . (!!0). readHex) . splits 2
  where tuple [x,y,z] = (x,y,z)

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  let (r,g,b) = stringToColor color
  xcolor <- allocColor dpy colormap $ Color 0 r g b $ doRed .|. doGreen .|. doBlue
  return $ color_pixel xcolor
