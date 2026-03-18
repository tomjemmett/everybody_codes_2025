module Stories.S1.Quest02Spec (spec) where

import SpecHelper
import Stories.S1.Quests

spec :: Spec
spec = describe "Story 1, Quest 2" $ do
  it "Sample" $ do
    s <- quest02 "sample"
    s `shouldBe` ("CFGNLK", "MGFLNK", "DJCGL")

  it "Actual" $ do
    a <- quest02 "actual"
    a `shouldBe` ("QUACK!HLFFJZHF", "QUACK!VPNGTBYBWMFTNL", "QUACK!PMVJPJGJNXHBSNZYXTMZJGZLNJVG")
