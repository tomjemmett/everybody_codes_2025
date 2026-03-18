module Events.Y2025.Quest10Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 10" $ do
  it "Sample" $ do
    s <- quest10 "sample"
    s `shouldBe` (27, 27, 13033988838)

  it "Actual" $ do
    a <- quest10 "actual"
    a `shouldBe` (143, 1749, 33411392898387)
