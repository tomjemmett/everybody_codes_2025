module Events.Y2024.Quest02Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 2" $ do
  it "Sample" $ do
    s <- quest02 "sample"
    s `shouldBe` (4, 42, 10)

  it "Actual" $ do
    a <- quest02 "actual"
    a `shouldBe` (31, 5213, 11385)
