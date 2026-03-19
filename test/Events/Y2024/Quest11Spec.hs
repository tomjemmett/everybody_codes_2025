module Events.Y2024.Quest11Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 11" $ do
  it "Sample" $ do
    s <- quest11 "sample"
    s `shouldBe` (8, 1, 268815)

  it "Actual" $ do
    a <- quest11 "actual"
    a `shouldBe` (34, 221560, 1570268426482)
