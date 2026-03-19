module Events.Y2024.Quest18Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 18" $ do
  it "Sample" $ do
    s <- quest18 "sample"
    s `shouldBe` (11, 21, 12)

  it "Actual" $ do
    a <- quest18 "actual"
    a `shouldBe` (107, 1337, 291737)
