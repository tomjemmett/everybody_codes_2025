module Events.Y2025.Quest11Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 11" $ do
  it "Sample" $ do
    s <- quest11 "sample"
    s `shouldBe` (109, 1579, 1378)

  it "Actual" $ do
    a <- quest11 "actual"
    a `shouldBe` (269, 3090294, 135000728971927)
