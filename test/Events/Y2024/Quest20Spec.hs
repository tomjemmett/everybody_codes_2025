module Events.Y2024.Quest20Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 20" $ do
  it "Sample" $ do
    s <- quest20 "sample"
    s `shouldBe` (1045, 24, 768790)

  it "Actual" $ do
    a <- quest20 "actual"
    a `shouldBe` (1031, 554, 768796)
