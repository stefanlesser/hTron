module Main where

import Tron
import System.Console.ANSI
import System.IO

drawPixel :: Int -> Int -> IO ()
drawPixel x y = do
  setCursorPosition y x
  setSGR [SetColor Background Vivid White]
  putStr " "

getInput :: IO (Char)
getInput = do
  char <- getChar
  return char

nextState :: Position -> IO ()
nextState pos@(Position x y) = do
  gameLoop pos

-- game loop
gameLoop :: Position -> IO ()
gameLoop pos@(Position x y) = do
  -- some test output
  drawPixel x y

  -- process input character
  input <- getInput
  case input of
    'q' -> handleExit
    'w' -> nextState (Position x (y - 1))
    'a' -> nextState (Position (x - 1) y)
    's' -> nextState (Position x (y + 1))
    'd' -> nextState (Position (x + 1) y)
    _   -> nextState pos

-- handle exit
handleExit :: IO ()
handleExit = do
  setSGR [Reset]
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"

-- main function
main :: IO ()
main = do
  -- set up terminal
  hSetEcho      stdin  False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "hTron"

  -- clear screen and launch into game loop
  clearScreen
  gameLoop (Position 10 10)

