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
    it "going west turning left goes south" $ do
      turnLeft West `shouldBe` South

    it "going east turning left goes north" $ do
      turnLeft East `shouldBe` North

    it "going north turning left goes west" $ do
      turnLeft North `shouldBe` West

    it "going south turning left goes east" $ do
      turnLeft South `shouldBe` East

  describe "turnRight" $ do
    it "going west turning right goes north" $ do
      turnRight West `shouldBe` North

    it "going east turning right goes south" $ do
      turnRight East `shouldBe` South

    it "going north turning right goes east" $ do
      turnRight North `shouldBe` East

    it "going south turning right goes west" $ do
      turnRight South `shouldBe` West

  describe "move" $ do
    it "moving west equals Position(x - 1, y)" $ do
      property $ (\p@(Position x y) -> move West p == Position (x - 1) y)

    it "moving east equals Position(x + 1, y)" $ do
      property $ (\p@(Position x y) -> move East p == Position (x + 1) y)

    it "moving north equals Position(x, y - 1)" $ do
      property $ (\p@(Position x y) -> move North p == Position x (y - 1))

    it "moving south equals Position(x, y + 1)" $ do
      property $ (\p@(Position x y) -> move South p == Position x (y + 1))

  describe "movePlayer" $ do
    it "move player moves player in its direction" $ do
      movePlayer (Player (Position 10 10) East) `shouldBe` Player (Position 11 10) East
    -- HELP: Is there a good way to test player movement with randomized QuickCheck?

  describe "turnPlayer" $ do
    it "turn player left changes player's direction" $ do
      turnPlayerLeft (Player (Position 10 10) South) `shouldBe` Player (Position 10 10) East

    it "turn player right changes player's direction" $ do
      turnPlayerRight (Player (Position 10 10) South) `shouldBe` Player (Position 10 10) West
    -- HELP: Is there a good way to test player changing direction with randomized QuickCheck?

