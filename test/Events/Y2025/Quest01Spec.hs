module Events.Y2025.Quest01Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 1" $ do
  it "Sample" $ do
    s <- quest01 "sample"
    s `shouldBe` ("Fyrryn", "Elarzris", "Drakzyph")

  it "Actual" $ do
    a <- quest01 "actual"
    a `shouldBe` ("Narcoryx", "Dorxith", "Uryth")
