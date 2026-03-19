module Events.Y2024.Quest09Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 9" $ do
  it "Sample" $ do
    s <- quest09 "sample"
    s `shouldBe` (10, 10, 10449)

  it "Actual" $ do
    a <- quest09 "actual"
    a `shouldBe` (13037, 4926, 153761)
