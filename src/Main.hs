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
gameLoop :: Position -> IO ()
gameLoop pos@(Position x y) = do
  -- some test output
  drawPixel x y

  -- process input character
  input <- timeout 100000 getChar
  case input of
    Just 'q' -> handleExit
    Just 'w' -> gameLoop (Position x (y - 1))
    Just 'a' -> gameLoop (Position (x - 1) y)
    Just 's' -> gameLoop (Position x (y + 1))
    Just 'd' -> gameLoop (Position (x + 1) y)
    Just _   -> gameLoop pos
    Nothing  -> gameLoop (Position x (y + 1))

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

