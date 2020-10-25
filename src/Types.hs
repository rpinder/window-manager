module Types where

import Control.Monad.State
import Graphics.X11.Xlib
import qualified Data.Map as M

type X a = StateT Xstate IO a

data Xstate = Xstate
            { display :: Display
            , root :: Window
            , keybinds :: M.Map (KeyCode, KeyMask) Action
            , focused :: Maybe Client
            , dragging :: !(Maybe (Position -> Position -> X ()))
            , windows :: ![Client]
            , quit :: Bool
            , borderFocusedPixel :: Pixel
            , borderUnfocusedPixel :: Pixel
            }

data Client = Client
            { c_x :: Position 
            , c_y :: Position
            , c_width :: Dimension
            , c_height :: Dimension
            , c_window :: Window
            }
            deriving (Eq, Show)

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

data Config = Config
              { borderWidth :: Int
              , borderFocusedColor :: String
              , borderUnfocusedColor :: String
              , step :: Int
              }
