module Tron where

data Direction
  = North
  | South
  | West
  | East
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

