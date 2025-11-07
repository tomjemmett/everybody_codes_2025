module Day02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 2" $ do
  it "Sample" $ do
    s <- day02 "sample"
    s `shouldBe` ("[357,862]", 4076, 406954)

  it "Actual" $ do
    a <- day02 "actual"
    a `shouldBe` ("[268294,973842]", 1047, 98316)