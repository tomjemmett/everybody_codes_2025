module Events.Y2025.Quest06Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 6" $ do
  it "Sample" $ do
    s <- quest06 "sample"
    s `shouldBe` (5, 11, 3442321)

  it "Actual" $ do
    a <- quest06 "actual"
    a `shouldBe` (137, 4223, 1665521085)
