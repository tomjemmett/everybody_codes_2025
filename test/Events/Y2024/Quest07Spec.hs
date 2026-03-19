module Events.Y2024.Quest07Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 7" $ do
  it "Sample" $ do
    s <- quest07 "sample"
    s `shouldBe` ("BDCA", "DCBA", 9240)

  it "Actual" $ do
    a <- quest07 "actual"
    a `shouldBe` ("BHAJDGIKF", "HDEKCGIFJ", 4226)
