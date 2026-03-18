module Events.Y2025.Quest16Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 16" $ do
  it "Sample" $ do
    s <- quest16 "sample"
    s `shouldBe` (193, 270, 94439495762954)

  it "Actual" $ do
    a <- quest16 "actual"
    a `shouldBe` (207, 112513370112, 93567233166974)
