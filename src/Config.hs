module Config where

import Data.Tuple
import qualified Data.Map as M
import Graphics.X11.Xlib
import Data.Bits
import Numeric
import Data.Word

import Types
import Utils
import Parser
import qualified Data.Text as T
import Data.Either (rights, lefts)

-- keybindsmap :: M.Map Action (String, KeyMask)
-- keybindsmap = M.fromList [ (MoveLeft, ("h", mod1Mask))
--                       , (MoveDown, ("j", mod1Mask))
--                       , (MoveUp, ("k", mod1Mask))
--                       , (MoveRight, ("l", mod1Mask))
--                       , (DecreaseWidth, ("h", mod1Mask .|. shiftMask))
--                       , (IncreaseHeight, ("j", mod1Mask .|. shiftMask))
--                       , (DecreaseHeight, ("k", mod1Mask .|. shiftMask))
--                       , (IncreaseWidth, ("l", mod1Mask .|. shiftMask))
--                       , (Launch "dmenu_run", ("d", mod1Mask))
--                       , (Raise, ("r", mod1Mask))
--                       , (CloseWindow, ("c", mod1Mask))
--                       , (Quit, ("q", mod1Mask .|. shiftMask))
--                       , (SwitchToWorkspace 0, ("1", mod1Mask))
--                       , (SwitchToWorkspace 1, ("2", mod1Mask))
--                       , (SwitchToWorkspace 2, ("3", mod1Mask))
--                       , (SwitchToWorkspace 3, ("4", mod1Mask))
--                       , (SwitchToWorkspace 4, ("5", mod1Mask))
--                       , (SwitchToWorkspace 5, ("6", mod1Mask))
--                       , (SwitchToWorkspace 6, ("7", mod1Mask))
--                       , (SwitchToWorkspace 7, ("8", mod1Mask))
--                       , (SwitchToWorkspace 8, ("9", mod1Mask))
--                       ]

-- keysyms :: Display -> IO (M.Map (KeyCode, KeyMask) Action)
-- keysyms dpy = do
--   let f (a, b) = do
--         code <- keysymToKeycode dpy $ stringToKeysym a
--         return (code, b)
--   m <- sequence $ M.map f keybindsmap
--   return $ mapconvert m

keysequence :: Ord a => M.Map (IO a) b -> IO (M.Map a b)
keysequence = M.foldrWithKey f $ pure M.empty
    where f key val mb = do
            k <- key
            b <- mb
            return $ M.insert k val b

stringToKeyCode :: Display -> M.Map (String, KeyMask) Action -> IO (M.Map (KeyCode, KeyMask) Action)
stringToKeyCode dpy keybinds = do
  m <- sequence . M.map f $ mapconvert keybinds
  return $ mapconvert m
  where
    f (a, b) = do
      code <- keysymToKeycode dpy $ stringToKeysym a
      return (code, b)

mapconvert :: Ord a => M.Map k a -> M.Map a k
mapconvert = M.fromList . map swap . M.toList

step :: Num a => a
step = 15

stringToColor :: String -> (Word16, Word16, Word16)
stringToColor = tuple . map (*257) . map (fst . (!!0). readHex) . splits 2
  where tuple [x,y,z] = (x,y,z)

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  let (r,g,b) = stringToColor color
  xcolor <- allocColor dpy colormap $ Color 0 r g b $ doRed .|. doGreen .|. doBlue
  return $ color_pixel xcolor

cleanup :: [String] -> [String]
cleanup = filter (\x -> x!!0 /= '#') . filter (/="")

readConfig :: FilePath -> IO Config
readConfig f = do
  file <- cleanup . lines <$> readFile f
  let errors = lefts $ map (\x -> parseLine x f) file
  mapM_ putStr errors
  let commands =  rights $ map (\x -> parseLine x f) file
  return $ Config (combineKeybinds $ k commands)
                  (combineSettings $ s commands)
  where
    s xs = [a | S a <- xs]
    k xs = [a | K a <- xs]

combineKeybinds :: [Keybind] -> M.Map (String, KeyMask) Action
combineKeybinds = mconcat <$> map convertKeybind
  where convertKey (Key t km) = (T.unpack t, km)
        convertKeybind (Keybind k a) = M.singleton (convertKey k) a

combineSettings :: [Setting] -> M.Map COptions ResultValue
combineSettings = mconcat <$> map convertSetting
  where convertSetting (Setting copt rv) = M.singleton copt rv
