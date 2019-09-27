module Main where

import           Test.Tasty

import           ParseTest

tests :: TestTree
tests = testGroup "1# Tests" [parseTests]

main :: IO ()
main = defaultMain tests
