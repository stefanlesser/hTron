module Main where

import Tron
import System.Console.ANSI
import System.IO
import System.Timeout

drawPixel :: Int -> Int -> IO ()
drawPixel x y = do
  setCursorPosition y x
  setSGR [SetColor Background Vivid White]
  putStr " "

-- game loop
gameLoop :: Player -> IO ()
gameLoop player@(Player pid (Position x y) direction) = do
  -- some test output
  drawPixel x y

  -- process input character
  input <- timeout 100000 getChar
  case input of
    Just 'q' -> handleExit
    Just 'z' -> gameLoop $ movePlayer $ turnPlayerLeft player 
    Just 'x' -> gameLoop $ movePlayer $ turnPlayerRight player
    Just _   -> gameLoop $ movePlayer player
    Nothing  -> gameLoop $ movePlayer player

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
  gameLoop $ Player 1 (Position 10 10) East

