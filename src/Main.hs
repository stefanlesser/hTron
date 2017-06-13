module Main where

import Tron
import System.Console.ANSI
import System.IO
import System.Timeout
import System.Console.Terminal.Size

-- drawing
drawPixel :: Int -> Int -> IO ()
drawPixel x y = do
  setCursorPosition y x
  setSGR [SetColor Background Vivid Green]
  putStr " "

drawPlayer :: Player -> IO ()
drawPlayer (Player _ (Position x y) _) = drawPixel x y

drawStep :: Step -> IO ()
drawStep (Step players) = mapM_ drawPlayer players 

-- game loop
gameLoop :: Configuration -> Step -> IO ()
gameLoop config step = do
  -- rendering
  drawStep step

  -- process input character
  input <- timeout 100000 getChar
  case input of
    Just 'y'  -> handleExit
    Just '2'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action LeftTurn  1 ] step
    Just 'q'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action RightTurn 1 ] step
    Just 'a'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action LeftTurn  2 ] step
    Just 'z'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action RightTurn 2 ] step
    Just 'c'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action LeftTurn  3 ] step
    Just 'v'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action RightTurn 3 ] step
    Just 'n'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action LeftTurn  4 ] step
    Just 'm'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action RightTurn 4 ] step
    Just '/'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action LeftTurn  5 ] step
    Just '\'' -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action RightTurn 5 ] step
    Just ']'  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action LeftTurn  6 ] step
    Just '='  -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [ Action RightTurn 6 ] step
    Just _    -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [] step
    Nothing   -> gameLoop config $ filterOffGridPlayers (gridWidth config, gridHeight config) $ tickStep [] step

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
  gameLoop config $ initializePlayers (gridWidth config, gridHeight config) 6

