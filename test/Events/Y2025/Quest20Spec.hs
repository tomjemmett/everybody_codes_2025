module Events.Y2025.Quest20Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 20" $ do
  it "Sample" $ do
    s <- quest20 "sample"
    s `shouldBe` (7, 32, 23)

  it "Actual" $ do
    a <- quest20 "actual"
    a `shouldBe` (120, 571, 474)
