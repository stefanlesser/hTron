module Main where

import Tron
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Position where
  arbitrary = positionGen

positionGen :: Gen Position
positionGen = do
  x <- arbitrary
  y <- arbitrary
  return (Position x y)

main :: IO ()
main = hspec $ do
  describe "turnLeft" $ do
    it "going west turning left goes south" $
      turnLeft West `shouldBe` South

    it "going east turning left goes north" $
      turnLeft East `shouldBe` North

    it "going north turning left goes west" $
      turnLeft North `shouldBe` West

    it "going south turning left goes east" $
      turnLeft South `shouldBe` East

  describe "turnRight" $ do
    it "going west turning right goes north" $
      turnRight West `shouldBe` North

    it "going east turning right goes south" $
      turnRight East `shouldBe` South

    it "going north turning right goes east" $
      turnRight North `shouldBe` East

    it "going south turning right goes west" $
      turnRight South `shouldBe` West

  describe "move" $ do
    it "moving west equals Position(x - 1, y)" $
      property (\p@(Position x y) -> move West p == Position (x - 1) y)

    it "moving east equals Position(x + 1, y)" $
      property (\p@(Position x y) -> move East p == Position (x + 1) y)

    it "moving north equals Position(x, y - 1)" $
      property (\p@(Position x y) -> move North p == Position x (y - 1))

    it "moving south equals Position(x, y + 1)" $
      property (\p@(Position x y) -> move South p == Position x (y + 1))

  describe "movePlayer" $ do
    it "move player moves player in its direction" $
      movePlayer (Player 1 (Position 10 10) East) `shouldBe` Player 1 (Position 11 10) East
    -- HELP: Is there a good way to test player movement with randomized QuickCheck?

  describe "turnPlayer" $ do
    it "turn player left changes player's direction" $
      turnPlayerLeft (Player 1 (Position 10 10) South) `shouldBe` Player 1 (Position 10 10) East

    it "turn player right changes player's direction" $
      turnPlayerRight (Player 1 (Position 10 10) South) `shouldBe` Player 1 (Position 10 10) West
    -- HELP: Is there a good way to test player changing direction with randomized QuickCheck?

  describe "applyAction" $ do
    it "does not change the player if the action is not for the player" $
      let player = (Player 2 (Position 20 20) West) in
      applyAction (TurnLeft 1) player `shouldBe` player

    it "changes the player if the action is for the player" $
      let player = (Player 1 (Position 20 20) West) in
      applyAction (TurnLeft 1) player `shouldBe` Player 1 (Position 20 20) South


  describe "tick" $ do
    it "tick generates the next state" $
      let players = [Player 1 (Position 10 10) East, Player 2 (Position 20 20) West] 
          actions = [] in
        (tick players actions) `shouldBe`
          [Player 1 (Position 11 10) East, Player 2 (Position 19 20) West]

    it "tick runs actions to generate next state" $
      let 
        players = [Player 1 (Position 10 10) East, Player 2 (Position 20 20) West] 
        actions = [TurnLeft 1, TurnRight 2] 
      in
        (tick players actions) `shouldBe`
          [Player 1 (Position 10 9) North, Player 2 (Position 20 19) North]
