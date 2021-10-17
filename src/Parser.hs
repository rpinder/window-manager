{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits

import Types
import Graphics.X11.Types
import Data.Void

type Parser = Parsec Void Text

data Line = S Setting | K Keybind
  deriving (Eq, Show)

data Keybind = Keybind Key Action
  deriving (Eq, Show)

data Key = Key Text KeyMask
  deriving (Eq, Show)

data Setting  = Setting COptions ResultValue
  deriving (Eq, Show)

sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

pLine :: Parser Line
pLine = S <$> pSetting <|> K <$> pKeybind

parseLine :: String -> FilePath -> Either String Line
parseLine str fp = case runParser pLine fp (T.pack str) of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

charToKeyMask :: Char -> KeyMask
charToKeyMask 'S' = mod4Mask
charToKeyMask 'C' = controlMask
charToKeyMask 'A' = mod1Mask
charToKeyMask 's' = shiftMask

pKeys :: Parser Key
pKeys = do
  c <- some $ char 'S' <|> char 'C' <|> char 'A' <|> char 's'
  void (symbol "-")
  l <- lexeme $ some alphaNumChar
  return $ Key (T.pack l) (combineKeyMasks c)
  where combineKeyMasks :: String -> KeyMask
        combineKeyMasks = foldr1 (.|.) . map charToKeyMask

pKeybind :: Parser Keybind
pKeybind = do
  keys <- pKeys <?> "Keys"
  void (symbol "|")
  act <- pAction
  return $ Keybind keys act

pSetting :: Parser Setting
pSetting = do
  option <- pCOption <?> "Config Option"
  void (symbol "=")
  val <- case option of
           BorderWidth -> Number <$> integer <?> "number"
           Step -> Number <$> integer <?> "number"
           _ -> Colour <$> T.pack <$> (char '#' *> some alphaNumChar <?> "colour")
  return $ Setting option val

pActionInput :: Parser a -> String -> (a -> Action) -> Parser Action
pActionInput p s a = do
  void (symbol $ T.pack s)
  cmd <- p <?> s ++ "Command"
  return $ a cmd

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')
 
pActionCmd :: String -> (String -> Action) -> Parser Action
pActionCmd = pActionInput stringLiteral

pActionNumber :: String -> (Int -> Action) -> Parser Action
pActionNumber = pActionInput integer
   
pAction :: Parser Action
pAction = choice
  [ MoveLeft <$ symbol "MoveLeft"
  , MoveUp <$ symbol "MoveUp"
  , MoveDown <$ symbol "MoveDown"
  , MoveRight <$ symbol "MoveRight"
  , IncreaseWidth <$ symbol "IncreaseWidth"
  , DecreaseWidth <$ symbol "DecreaseWidth"
  , IncreaseHeight <$ symbol "IncreaseHeight"
  , DecreaseHeight <$ symbol "DecreaseHeight"
  , Raise <$ symbol "Raise"
  , Quit <$ symbol "Quit"
  , CloseWindow <$ symbol "Close"
  , pActionCmd "Launch" Launch
  , pActionNumber "Workspace" SwitchToWorkspace ]

pCOption :: Parser COptions
pCOption = choice
  [ BorderWidth            <$ symbol "borderWidth"
  , CColor BorderFocused   <$ symbol "borderFocused"
  , CColor BorderUnfocused <$ symbol "borderUnfocused"
  , Step                   <$ symbol "step" ]
