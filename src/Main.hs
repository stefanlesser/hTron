module Main where

import Tron
import System.Console.ANSI
import System.IO
import System.Timeout
import System.Console.Terminal.Size

data Configuration = Configuration
  { players    :: Int
  , gridWidth  :: Int
  , gridHeight :: Int
  }

-- drawing
drawPixel :: Int -> Int -> IO ()
drawPixel x y = do
  setCursorPosition y x
  setSGR [SetColor Background Vivid White]
  putStr " "

drawPlayer :: Player -> IO ()
drawPlayer (Player _ (Position x y) _) = drawPixel x y

drawStep :: Step -> IO ()
drawStep (Step players) = mapM_ drawPlayer players 

-- game loop
gameLoop :: Step -> IO ()
gameLoop step = do
  -- some test output
  drawStep step

  -- process input character
  input <- timeout 100000 getChar
  case input of
    Just 'q' -> handleExit
    Just 'z' -> gameLoop $ tickStep [ Action LeftTurn  1 ] step
    Just 'x' -> gameLoop $ tickStep [ Action RightTurn 1 ] step
    Just _   -> gameLoop $ tickStep [] step
    Nothing  -> gameLoop $ tickStep [] step

-- set up terminal / screen; returns screen dimensions
handleStartup :: IO Configuration
handleStartup = do
  hSetEcho      stdin  False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "hTron"
  window <- size
  case window of
    Just (Window height width) -> return (Configuration 2 width height)

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
  config <- handleStartup
  clearScreen
  putStr $ "Grid size: " ++ show (gridWidth config) ++ ", " ++ show (gridHeight config)
  gameLoop $ initializePlayers (gridWidth config, gridHeight config) 6

