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
    Just '2'  -> continueWithActions [Action LeftTurn  (PlayerId 1)]
    Just 'q'  -> continueWithActions [Action RightTurn (PlayerId 1)]
    Just 'a'  -> continueWithActions [Action LeftTurn  (PlayerId 2)]
    Just 'z'  -> continueWithActions [Action RightTurn (PlayerId 2)]
    Just 'c'  -> continueWithActions [Action LeftTurn  (PlayerId 3)]
    Just 'v'  -> continueWithActions [Action RightTurn (PlayerId 3)]
    Just 'n'  -> continueWithActions [Action LeftTurn  (PlayerId 4)]
    Just 'm'  -> continueWithActions [Action RightTurn (PlayerId 4)]
    Just '/'  -> continueWithActions [Action LeftTurn  (PlayerId 5)]
    Just '\'' -> continueWithActions [Action RightTurn (PlayerId 5)]
    Just ']'  -> continueWithActions [Action LeftTurn  (PlayerId 6)]
    Just '='  -> continueWithActions [Action RightTurn (PlayerId 6)]
    Just _    -> continueWithActions []
    Nothing   -> continueWithActions []
  where
    continueWithActions a = gameLoop config 
                          $ filterOffGridPlayers (gridWidth config, gridHeight config) 
                          $ tickStep a step

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

