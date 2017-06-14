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

newtype PlayerId = PlayerId Int
  deriving (Eq, Show)

data Player = Player 
  { playerId  :: PlayerId
  , position  :: Position
  , direction :: Direction
  }
  deriving (Show, Eq)

newtype Step = Step [Player]
  deriving (Show, Eq)

data Configuration = Configuration
  { numPlayers :: Int
  , gridWidth  :: Int
  , gridHeight :: Int
  }
  deriving (Show, Eq)

data World = World Configuration [Step]
  deriving (Show, Eq)

data Action = Action Turn PlayerId
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
applyAction (Action turnDirection actionPid) p@(Player pid _ _) 
  | actionPid == pid = turnPlayer turnDirection p 
  | otherwise        = p

applyActionToStep :: Action -> Step -> Step
applyActionToStep action (Step players) = Step $ applyAction action <$> players

applyActionsToStep :: [Action] -> Step -> Step
applyActionsToStep actions step = foldr applyActionToStep step actions

-- collision detection
isPlayerOnGrid :: (Int, Int) -> Player -> Bool
isPlayerOnGrid (width, height) player
  | getX (position player) < 0       = False
  | getX (position player) >= width  = False
  | getY (position player) < 0       = False
  | getY (position player) >= height = False
  | otherwise                           = True

didPlayerHitWall :: (Int, Int) -> Player -> Bool
didPlayerHitWall = undefined

filterOffGridPlayers :: (Int, Int) -> Step -> Step
filterOffGridPlayers size (Step players) = Step $ filter (isPlayerOnGrid size) players

-- game logic combined
tickStep :: [Action] -> Step -> Step
tickStep actions = nextStep . applyActionsToStep actions

tickWorld :: [Action] -> World -> World
tickWorld actions (World config steps) = World config $ steps ++ [ tickStep actions $ last steps ]

-- player placement
initializePlayers :: (Int, Int) -> Int -> Step
initializePlayers (width, height) _ =
  Step [ Player (PlayerId 1) (Position (width `div` 4    ) (height `div` 6    )) East
       , Player (PlayerId 2) (Position (width `div` 4 * 3) (height `div` 6    )) West
       , Player (PlayerId 3) (Position (width `div` 4    ) (height `div` 6 * 3)) East
       , Player (PlayerId 4) (Position (width `div` 4 * 3) (height `div` 6 * 3)) West
       , Player (PlayerId 5) (Position (width `div` 4    ) (height `div` 6 * 5)) East
       , Player (PlayerId 6) (Position (width `div` 4 * 3) (height `div` 6 * 5)) West
       ]
