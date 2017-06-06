module Tron where

data Direction
  = North
  | South
  | West
  | East
  deriving (Show, Eq)

data Position = Position Int Int
  deriving (Show, Eq)

data Player = Player Int Position Direction
  deriving (Show, Eq)

data Action
  = TurnLeft Int
  | TurnRight Int
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

movePlayer :: Player -> Player
movePlayer (Player pid pos d) = Player pid (move d pos) d

turnPlayerLeft :: Player -> Player
turnPlayerLeft (Player pid pos d) = Player pid pos (turnLeft d)

turnPlayerRight :: Player -> Player
turnPlayerRight (Player pid pos d) = Player pid pos (turnRight d)

tick :: [Player] -> [Action] -> [Player]
tick players actions = map movePlayer $ map applyActionsToPlayer players
  where
    applyActionsToPlayer p = foldr (\action acc -> applyAction action acc) p actions 

applyAction :: Action -> Player -> Player
applyAction (TurnLeft actionPid) p@(Player pid _ _) = if (actionPid == pid) then turnPlayerLeft p else p
applyAction (TurnRight actionPid) p@(Player pid _ _) = if (actionPid == pid) then turnPlayerRight p else p
