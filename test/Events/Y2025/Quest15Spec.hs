module Events.Y2025.Quest15Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 15" $ do
  it "Sample" $ do
    s <- quest15 "sample"
    s `shouldBe` (16, 16, 16)

  it "Actual" $ do
    a <- quest15 "actual"
    a `shouldBe` (100, 4529, 402304612)
