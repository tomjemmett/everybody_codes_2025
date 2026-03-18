module Events.Y2025.Quest03Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 3" $ do
  it "Sample" $ do
    s <- quest03 "sample"
    s `shouldBe` (29, 781, 3)

  it "Actual" $ do
    a <- quest03 "actual"
    a `shouldBe` (2633, 297, 4485)
