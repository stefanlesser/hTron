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

data Position = Position 
  { getX :: Int
  , getY :: Int
  }
  deriving (Show, Eq)

data Player = Player 
  { getNumber    :: Int
  , getPosition  :: Position
  , getDirection :: Direction
  }
  deriving (Show, Eq)

newtype Step = Step [Player]
  deriving (Show, Eq)

data Configuration = Configuration
  { getPlayers :: Int
  , gridWidth  :: Int
  , gridHeight :: Int
  }
  deriving (Show, Eq)

data World = World Configuration [Step]
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
nextStep (Step players) = Step $ movePlayer <$> players

-- changing direction
turnPlayer :: Turn -> Player -> Player
turnPlayer turnDirection (Player pid pos d) = Player pid pos (turn turnDirection d)

applyAction :: Action -> Player -> Player
applyAction (Action turnDirection actionPid) p@(Player pid _ _) = if actionPid == pid then turnPlayer turnDirection p else p

applyActionToStep :: Action -> Step -> Step
applyActionToStep action (Step players) = Step $ applyAction action <$> players

applyActionsToStep :: [Action] -> Step -> Step
applyActionsToStep actions step = foldr applyActionToStep step actions

-- collision detection
didPlayerLeaveGrid :: World -> Player -> Bool
didPlayerLeaveGrid (World config _) player
  | getX (getPosition player) < 0                  = True
  | getX (getPosition player) >= gridWidth config  = True
  | getY (getPosition player) < 0                  = True
  | getY (getPosition player) >= gridHeight config = True
  | otherwise                                      = False

didPlayerHitWall :: World -> Player -> Bool
didPlayerHitWall = undefined

-- game logic combined
tickStep :: [Action] -> Step -> Step
tickStep actions = nextStep . applyActionsToStep actions

tickWorld :: [Action] -> World -> World
tickWorld actions (World config steps) = World config $ steps ++ [ tickStep actions $ last steps ]

-- player placement

initializePlayers :: (Int, Int) -> Int -> Step
initializePlayers (width, height) _ =
  Step [ Player 1 (Position (width `div` 4    ) (height `div` 6    )) East
       , Player 2 (Position (width `div` 4 * 3) (height `div` 6    )) West
       , Player 3 (Position (width `div` 4    ) (height `div` 6 * 3)) East
       , Player 4 (Position (width `div` 4 * 3) (height `div` 6 * 3)) West
       , Player 5 (Position (width `div` 4    ) (height `div` 6 * 5)) East
       , Player 6 (Position (width `div` 4 * 3) (height `div` 6 * 5)) West
       ]
