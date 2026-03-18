module Events.Y2025.Quest02Spec (spec) where

import Events.Y2025.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 2" $ do
  it "Sample" $ do
    s <- quest02 "sample"
    s `shouldBe` ("[357,862]", 4076, 406954)

  it "Actual" $ do
    a <- quest02 "actual"
    a `shouldBe` ("[268294,973842]", 1047, 98316)
