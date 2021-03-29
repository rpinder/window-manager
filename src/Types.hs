module Types where

import Control.Monad.State
import Graphics.X11.Xlib
import qualified Data.Map as M
import qualified Data.Vector as V

type X a = StateT Xstate IO a

data Xstate = Xstate
            { display :: Display
            , root :: Window
            , keybinds :: M.Map (KeyCode, KeyMask) Action
            , focused :: Maybe Int
            , dragging :: !(Maybe (Position -> Position -> X ()))
            , workspaces :: V.Vector [Client]
            , current_ws :: Int
            , quit :: Bool
            , colors :: M.Map String Pixel
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
            | Quit
            | CloseWindow
            | SwitchToWorkspace Int
            | None
            deriving (Eq, Ord, Show)

data Config = Config
              { borderWidth :: Int
              , borderFocusedColor :: String
              , borderUnfocusedColor :: String
              , step :: Int
              }
