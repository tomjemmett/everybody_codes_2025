module Events.Y2025.Quest12Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 12" $ do
  it "Sample" $ do
    s <- quest12 "sample"
    s `shouldBe` (16, 58, 14)

  it "Actual" $ do
    a <- quest12 "actual"
    a `shouldBe` (242, 5678, 4098)
