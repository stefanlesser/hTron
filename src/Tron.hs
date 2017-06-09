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

newtype Step = Step [Player]
  deriving (Show, Eq)

newtype World = World [Step]
  deriving (Show, Eq)

data Action = Action Turn Int
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

nextStep :: Step -> Step
nextStep (Step players) = Step $ map movePlayer players

-- changing direction
turnPlayer :: Turn -> Player -> Player
turnPlayer LeftTurn  (Player pid pos d) = Player pid pos (turn LeftTurn d)
turnPlayer RightTurn (Player pid pos d) = Player pid pos (turn RightTurn d)

applyAction :: Action -> Player -> Player
applyAction (Action turnDirection actionPid) p@(Player pid _ _) = if actionPid == pid then turnPlayer turnDirection p else p

applyActionToStep :: Action -> Step -> Step
applyActionToStep action (Step players) = Step $ map (applyAction action) players

applyActionsToStep :: [Action] -> Step -> Step
applyActionsToStep actions step = foldr applyActionToStep step actions

-- game logic combined
tickStep :: [Action] -> Step -> Step
tickStep actions = nextStep . applyActionsToStep actions

tickWorld :: [Action] -> World -> World
tickWorld actions (World steps) = World $ steps ++ [ tickStep actions $ last steps ]

