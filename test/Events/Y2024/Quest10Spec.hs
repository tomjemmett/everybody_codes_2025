module Events.Y2024.Quest10Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 10" $ do
  it "Sample" $ do
    s <- quest10 "sample"
    s `shouldBe` ("PTBVRCZHFLJWGMNS", 1851, 3889)

  it "Actual" $ do
    a <- quest10 "actual"
    a `shouldBe` ("XHRFNBMZTSVQWJLP", 196798, 208713)
