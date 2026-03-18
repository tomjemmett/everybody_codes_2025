module Events.Y2025.Quest18Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 18" $ do
  it "Sample" $ do
    s <- quest18 "sample"
    s `shouldBe` (774, 324, 946)

  it "Actual" $ do
    a <- quest18 "actual"
    a `shouldBe` (2215338, 12081610449, 241487)
