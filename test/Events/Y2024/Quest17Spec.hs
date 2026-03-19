module Events.Y2024.Quest17Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 17" $ do
  it "Sample" $ do
    s <- quest17 "sample"
    s `shouldBe` (16, 16, 15624)

  it "Actual" $ do
    a <- quest17 "actual"
    a `shouldBe` (138, 1245, 4105165440)
