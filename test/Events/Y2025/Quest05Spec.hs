module Events.Y2025.Quest05Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 5" $ do
  it "Sample" $ do
    s <- quest05 "sample"
    s `shouldBe` (581078, 77053, 260)

  it "Actual" $ do
    a <- quest05 "actual"
    a `shouldBe` (4563685626, 8617568255445, 31928850)
