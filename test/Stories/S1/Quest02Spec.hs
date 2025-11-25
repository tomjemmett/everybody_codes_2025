module Stories.S1.Quest02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Story 1, Quest 2" $ do
  it "Sample" $ do
    s <- s1q2 "sample"
    s `shouldBe` ("CFGNLK", "MGFLNK", "DJCGL")

  it "Actual" $ do
    a <- s1q2 "actual"
    a `shouldBe` ("QUACK!HLFFJZHF", "QUACK!VPNGTBYBWMFTNL", "QUACK!PMVJPJGJNXHBSNZYXTMZJGZLNJVG")
