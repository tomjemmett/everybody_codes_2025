module Events.Y2024.Quest01Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` (5, 28, 30)

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` (1359, 5633, 27967)
