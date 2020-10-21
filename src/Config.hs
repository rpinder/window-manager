module Config where

import Data.Tuple
import qualified Data.Map as M
import Graphics.X11.Xlib
import Data.Bits

import Types

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

step :: Num a => a
step = 15
