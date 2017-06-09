module Main where

import Tron
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Position where
  arbitrary = positionGen where
    positionGen = do
      x <- arbitrary
      y <- arbitrary
      return (Position x y)

instance Arbitrary Direction where
  arbitrary = directionGen where
    directionGen = do
      oneof 
        [ return West
        , return East
        , return North
        , return South
        ]

main :: IO ()
main = hspec $ do
  describe "turn LeftTurn" $ do
    it "turns south if headed west" $
      turn LeftTurn West `shouldBe` South

    it "turns north if headed east" $
      turn LeftTurn East `shouldBe` North

    it "turns west if headed north" $
      turn LeftTurn North `shouldBe` West

    it "turns east if headed south" $
      turn LeftTurn South `shouldBe` East

  describe "turn RightTurn" $ do
    it "turns north if headed west" $
      turn RightTurn West `shouldBe` North

    it "turns south if headed east" $
      turn RightTurn East `shouldBe` South

    it "turns east if headed north" $
      turn RightTurn North `shouldBe` East

    it "turns west if headed south" $
      turn RightTurn South `shouldBe` West

  describe "move" $ do
    it "decreases x by 1 and doesn't change y" $
      property (\p@(Position x y) -> move West p == Position (x - 1) y)

    it "increases x by 1 and doesn't change y" $
      property (\p@(Position x y) -> move East p == Position (x + 1) y)

    it "doesn't change x and decreases y by 1" $
      property (\p@(Position x y) -> move North p == Position x (y - 1))

    it "doesn't change x and increases y by 1" $
      property (\p@(Position x y) -> move South p == Position x (y + 1))

  describe "movePlayer" $ do
    it "moves player in its direction" $ do
      property (\pos dir -> movePlayer (Player 1 pos dir) == Player 1 (move dir pos) dir)

  describe "turnPlayer" $ do
    it "turns player counter-clockwise" $
      turnPlayer LeftTurn (Player 1 (Position 10 10) South) `shouldBe` Player 1 (Position 10 10) East

    it "turns player clockwise" $
      turnPlayer RightTurn (Player 1 (Position 10 10) South) `shouldBe` Player 1 (Position 10 10) West
    -- HELP: Is there a good way to test player changing direction with randomized QuickCheck?

  describe "nextStepState" $ do
    it "moves both players in their directions" $ do
      let step = Step [ Player 1 (Position 10 10) East, Player 2 (Position 20 20) West ] in
        nextStep step `shouldBe` Step [ Player 1 (Position 11 10) East, Player 2 (Position 19 20) West ]

  describe "applyActionToStep" $ do
    it "changes direction of player 1" $ do
      let step = Step [ Player 1 (Position 10 10) East, Player 2 (Position 20 20) West ]
          action = TurnLeft 1 in
        applyActionToStep action step `shouldBe` Step [ Player 1 (Position 10 10) North, Player 2 (Position 20 20) West ] 

  describe "applyActionsToStep" $ do
    it "changes directions of two players" $ do
      let step = Step [ Player 1 (Position 10 10) East, Player 2 (Position 20 20) West ]
          actions = [ TurnLeft 1, TurnRight 2 ] in
        applyActionsToStep actions step `shouldBe` Step [ Player 1 (Position 10 10) North, Player 2 (Position 20 20) North ] 

  describe "applyAction" $ do
    it "doesn't affect player if player ids don't match" $
      let player = Player 2 (Position 20 20) West in
      applyAction (TurnLeft 1) player `shouldBe` player

    it "changes player's direction if player ids match" $
      let player = Player 1 (Position 20 20) West in
      applyAction (TurnLeft 1) player `shouldBe` Player 1 (Position 20 20) South

  describe "tickStep" $ do
    it "generates next state without any actions" $
      let step = Step [ Player 1 (Position 10 10) East, Player 2 (Position 20 20) West ] 
          actions = [] in
        tickStep actions step `shouldBe` Step [ Player 1 (Position 11 10) East, Player 2 (Position 19 20) West ]

    it "applies actions and generates next state" $
      let step = Step [ Player 1 (Position 10 10) East, Player 2 (Position 20 20) West ]
          actions = [TurnLeft 1, TurnRight 2] in
        tickStep actions step `shouldBe` Step [ Player 1 (Position 10 9) North, Player 2 (Position 20 19) North ]

  describe "tickWorld" $ do
    it "accumulates steps for second round with no actions" $
      let world = World [ Step [ Player 1 (Position 10 10) South, Player 2 (Position 20 20) North ] ] in
        tickWorld [] world `shouldBe` World [ Step [ Player 1 (Position 10 10) South, Player 2 (Position 20 20) North ]
                                            , Step [ Player 1 (Position 10 11) South, Player 2 (Position 20 19) North ] ] 

    it "accumulates steps for second round with actions" $
      let actions = [ TurnLeft 2 ]
          world = World [ Step [ Player 1 (Position 10 10) South, Player 2 (Position 20 20) North ] ] in
        tickWorld actions world `shouldBe` World [ Step [ Player 1 (Position 10 10) South, Player 2 (Position 20 20) North ]
                                                 , Step [ Player 1 (Position 10 11) South, Player 2 (Position 19 20) West ] ] 
