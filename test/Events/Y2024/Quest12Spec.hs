module Events.Y2024.Quest12Spec (spec) where

import Events.Y2024.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 12" $ do
  it "Sample" $ do
    s <- quest12 "sample"
    s `shouldBe` (13, 22, 11)

  it "Actual" $ do
    a <- quest12 "actual"
    a `shouldBe` (214, 20595, 731188)
