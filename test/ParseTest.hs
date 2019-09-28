{-# LANGUAGE OverloadedStrings #-}

module ParseTest
  ( parseTests
  )
where

import qualified Data.Text                     as T
import           Data.Either
import           Test.Tasty
import           Test.Tasty.HUnit
import           Parse
import           OSType

parseAssertions :: [Assertion]
parseAssertions =
  [ assertEqual (t <> " parsed incorrectly") (Right e)
      $ parseOneSharp "test" (T.pack t)
    | (t, e) <-
      [ ("1#"     , [PI 1 1])
      , ("11#"    , [PI 2 1])
      , ("1 #"    , [PI 1 1])
      , (" 1#"    , [PI 1 1])
      , ("1# "    , [PI 1 1])
      , ("1#1##1#", [PI 1 1, PI 1 2, PI 1 1])
      , ( "1#  \n 1#\n #  \n 1 #  111111##### 1#"
        , [PI 1 1, PI 1 2, PI 1 1, PI 6 5, PI 1 1]
        )
      ]
    ]
    <> [ assertBool (t <> " parsed correctly but should throw an error")
         $ isLeft
         . parseOneSharp "test"
         $ (T.pack t)
       | t <- ["1", "#", " 1 ", " # ", "1#1", "1#1#1", "1######"]
       ]


parseTests :: TestTree
parseTests = testGroup
  "parse tests"
  [ testCase ("test " <> show i) t
  | (i, t) <- zip [(1 :: Int), 2 ..] parseAssertions
  ]
