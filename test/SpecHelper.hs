module SpecHelper
  ( module Test.Hspec,
    module System.IO,
  )
where

import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Test.Hspec
