module Events.Y2025.Quest09Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 9" $ do
  it "Sample" $ do
    s <- quest09 "sample"
    s `shouldBe` (414, 1245, 12)

  it "Actual" $ do
    a <- quest09 "actual"
    a `shouldBe` (5776, 317847, 37926)
