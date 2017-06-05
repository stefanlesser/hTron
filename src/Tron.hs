module Tron where

data Direction
  = North
  | South
  | West
  | East
  deriving (Show, Eq)

data Position = Position Int Int
  deriving (Show, Eq)

turnLeft :: Direction -> Direction
turnLeft West = South
turnLeft East = North
turnLeft North = West
turnLeft South = East

turnRight :: Direction -> Direction
turnRight West = North
turnRight East = South
turnRight North = East
turnRight South = West 

move :: Direction -> Position -> Position
move West  (Position x y) = Position (x - 1)  y
move East  (Position x y) = Position (x + 1)  y
move North (Position x y) = Position  x      (y - 1)
move South (Position x y) = Position  x      (y + 1)

