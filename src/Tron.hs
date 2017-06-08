module Tron where

data Direction
  = North
  | South
  | West
  | East
  deriving (Show, Eq)

data Turn
  = LeftTurn
  | RightTurn
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

turn :: Turn -> Direction -> Direction
turn LeftTurn  West  = South
turn LeftTurn  East  = North
turn LeftTurn  North = West
turn LeftTurn  South = East
turn RightTurn West  = North
turn RightTurn East  = South
turn RightTurn North = East
turn RightTurn South = West

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
turnPlayer :: Turn -> Player -> Player
turnPlayer LeftTurn  (Player pid pos d) = Player pid pos (turn LeftTurn d)
turnPlayer RightTurn (Player pid pos d) = Player pid pos (turn RightTurn d)

applyAction :: Action -> Player -> Player
applyAction (TurnLeft actionPid) p@(Player pid _ _) = if (actionPid == pid) then turnPlayer LeftTurn p else p
applyAction (TurnRight actionPid) p@(Player pid _ _) = if (actionPid == pid) then turnPlayer RightTurn p else p

applyActionToWorld :: Action -> World -> World
applyActionToWorld action (World players) = World $ map (\player -> applyAction action player) players

applyActionsToWorld :: [Action] -> World -> World
applyActionsToWorld actions world = foldr (\action world -> applyActionToWorld action world) world actions

-- game logic combined
tick :: [Action] -> World -> World
tick actions = nextWorldState . applyActionsToWorld actions

