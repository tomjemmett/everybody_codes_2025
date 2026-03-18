module Events.Y2025.Quest17Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 17" $ do
  it "Sample" $ do
    s <- quest17 "sample"
    s `shouldBe` (1573, 1090, 3180)

  it "Actual" $ do
    a <- quest17 "actual"
    a `shouldBe` (1632, 68328, 44042)
