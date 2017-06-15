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
drawStep = mapM_ drawPlayer

-- game loop
processInput :: Maybe Char -> [Action]
processInput (Just '2')  = [Action LeftTurn  (PlayerId 1)]
processInput (Just 'q')  = [Action RightTurn (PlayerId 1)]
processInput (Just 'a')  = [Action LeftTurn  (PlayerId 2)]
processInput (Just 'z')  = [Action RightTurn (PlayerId 2)]
processInput (Just 'c')  = [Action LeftTurn  (PlayerId 3)]
processInput (Just 'v')  = [Action RightTurn (PlayerId 3)]
processInput (Just 'n')  = [Action LeftTurn  (PlayerId 4)]
processInput (Just 'm')  = [Action RightTurn (PlayerId 4)]
processInput (Just '/')  = [Action LeftTurn  (PlayerId 5)]
processInput (Just '\'') = [Action RightTurn (PlayerId 5)]
processInput (Just ']')  = [Action LeftTurn  (PlayerId 6)]
processInput (Just '=')  = [Action RightTurn (PlayerId 6)]
processInput _           = []

gameLoop :: Configuration -> Step -> IO ()
gameLoop config players 
  | length players <= 1 = handleExit "Game over."
  | otherwise = do
  -- rendering
  drawStep players

  -- process input character
  input <- timeout 100000 getChar
  case input of 
    Just 'y' -> handleExit "Early exit. Bye bye."
    _        -> gameLoop config newStep
                  where newStep = filterOffGridPlayers (gridWidth config, gridHeight config) 
                                $ tickStep (processInput input) players
 
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
handleExit :: String -> IO ()
handleExit text = do
  setSGR [Reset]
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn text

-- main function
main :: IO ()
main = do
  config <- handleStartup
  clearScreen
  putStr $ "Grid size: " ++ show (gridWidth config) ++ ", " ++ show (gridHeight config)
  gameLoop config $ initializePlayers (gridWidth config, gridHeight config) 6

