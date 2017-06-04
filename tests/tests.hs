module Main where

import Tron
import Test.Hspec

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
