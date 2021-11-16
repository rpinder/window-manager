module Types where

import Control.Monad.State
import Graphics.X11.Xlib
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Text

type X a = StateT Xstate IO a

data Xstate = Xstate
            { display :: Display
            , root :: Window
            , keybinds :: M.Map (KeyCode, KeyMask) Action
            , settings :: M.Map COptions ResultValue
            , focused :: Maybe Int
            , dragging :: !(Maybe (Position -> Position -> X ()))
            , workspaces :: V.Vector [Client]
            , current_ws :: Int
            , quit :: Bool
            , colors :: M.Map ColorOption Pixel
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
            | Maximize
            | SwitchToWorkspace Int
            | None
            deriving (Eq, Ord, Show)

data Config = Config
              { _keybinds :: M.Map (String, KeyMask) Action
              , _settings :: M.Map COptions ResultValue
              } deriving (Eq, Show)



-- go through Coloroptions and have map of ColorOption to Pixel
data COptions
  = CColor ColorOption
  | Step
  | BorderWidth
  deriving (Eq, Show, Ord)

data ColorOption = BorderFocused | BorderUnfocused
  deriving (Eq, Show, Ord)

data ResultValue = Number Int | Colour Text
  deriving (Eq, Show)
