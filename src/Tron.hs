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

data World = World [Player]
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

-- moving
move :: Direction -> Position -> Position
move West  (Position x y) = Position (x - 1)  y
move East  (Position x y) = Position (x + 1)  y
move North (Position x y) = Position  x      (y - 1)
move South (Position x y) = Position  x      (y + 1)

movePlayer :: Player -> Player
movePlayer (Player pid pos d) = Player pid (move d pos) d

nextWorldState :: World -> World
nextWorldState (World players) = World $ map movePlayer players

-- changing direction
turnPlayerLeft :: Player -> Player
turnPlayerLeft (Player pid pos d) = Player pid pos (turnLeft d)

turnPlayerRight :: Player -> Player
turnPlayerRight (Player pid pos d) = Player pid pos (turnRight d)

applyAction :: Action -> Player -> Player
applyAction (TurnLeft actionPid) p@(Player pid _ _) = if (actionPid == pid) then turnPlayerLeft p else p
applyAction (TurnRight actionPid) p@(Player pid _ _) = if (actionPid == pid) then turnPlayerRight p else p

applyActionToWorld :: Action -> World -> World
applyActionToWorld action (World players) = World $ map (\player -> applyAction action player) players

applyActionsToWorld :: [Action] -> World -> World
applyActionsToWorld actions world = foldr (\action world -> applyActionToWorld action world) world actions

tick :: World -> [Action] -> World
tick world@(World players) actions = nextWorldState $ applyActionsToWorld actions world

