{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- BoolExpr unit tests
module Main where

import Data.Either (isRight)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Text.Parsec
import Test.Tasty.HUnit
import Data.BoolExpr
import Data.BoolExpr.Parser (parseBoolExpr, identifier)
import Data.BoolExpr.Simplify (cannotBeTrue)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Boolean Expressions" [
    parseabilityTests
  , matchingTests
  , simplificationTests
  ]

parseabilityTests :: TestTree
parseabilityTests = testGroup "Parseability" [
      testCase "Parsing a nontrivial expression"
        $ tryParse True "(a OR b) AND (n OR m) NOT (x OR y)"
    , testCase "Parsing an empty expression"
        $ tryParse False "()"
    ]
  where
    tryParse :: Bool -> String -> IO ()
    tryParse shouldBeParseable exprString =
        assertBool "Check whether expression is parseable" $ shouldBeParseable  == isRight p
      where
        p = runParser (parseBoolExpr identifier) () "" exprString


matchingTests :: TestTree
matchingTests = testGroup "Matching"
  [
    testCase "Simple query 1" $ tryQuery True query1 "I really like Haskell"
  , testCase "Simple query 2.1" $ tryQuery False query2 "I really like Haskell"
  , testCase "Simple query 2.2" $ tryQuery True query2 "You really like Haskell"
  ]
  where
    Right query1 = runParser (parseBoolExpr identifier) () "" "Haskell"
    Right query2 = runParser (parseBoolExpr identifier) () "" "Haskell NOT I"

    matchingString :: String -> BoolExpr String -> Bool
    matchingString doc = evalBoolExpr (`isInfixOf` doc)

    tryQuery :: Bool -> BoolExpr String -> String -> IO ()
    tryQuery expectedVal query input = assertBool "Check for match" $
      matchingString input query == expectedVal

simplificationTests =
  testGroup
    "Expression simplification"
    [ testGroup
        "Return true if the expression can be simplified to False"
        [ testGroup
            "Effect of constant literals"
            [ testCase
                "False input via single literal"
                $ expectTrue BFalse
            , testCase
                "True input via composed literals"
                $ expectFalse BTrue
            , testCase
                "False input via composed literals"
                $ expectTrue
                $ BOr BFalse BFalse
            , testCase
                "True input via composed literals"
                $ expectFalse
                $ BOr BFalse BTrue
            , testCase
                "Constant OR'd with False"
                $ expectFalse
                $ BOr BFalse (BConst (Positive "foo"))
            , testCase
                "Constant OR'd with True"
                $ expectFalse
                $ BOr (BConst (Positive "foo")) BTrue
            , testCase
                "Constant AND'd with False"
                $ expectTrue
                $ BAnd BFalse (BConst (Positive "foo"))
            , testCase
                "Constant AND'd with True"
                $ expectFalse
                $ BAnd (BConst (Positive "foo")) BTrue
            , testCase
                "Nested Constants AND'd with False within OR"
                $ expectTrue
                $ BOr
                  (BAnd BFalse (BConst (Positive "foo")))
                  (BAnd (BConst (Positive "bar")) BFalse)
            , testCase
                "Deeply nested Constants AND'd with False within OR with multiple negations"
                $ expectTrue
                $ BOr
                  (BAnd (BNot BTrue) (BNot (BNot (BNot (BConst (Positive "foo"))))))
                  (BAnd (BConst (Positive "bar")) (BNot (BNot BFalse)))
            ]
        , testGroup
            "Effect of contradicting named constants"
            [ testCase
                "via NOT operator"
                $ expectTrue
                $ BAnd (BNot (BConst (Positive "foo"))) (BConst (Positive "foo"))
            , testCase
                "via signedness"
                $ expectTrue
                $ BAnd (BConst (Positive "foo")) (BConst (Negative "foo"))
            ]
        ]
    ]
  where
    expectTrue, expectFalse :: BoolExpr String -> Assertion
    expectTrue = assertBool "Should have returned true" . cannotBeTrue
    expectFalse = assertBool "Should have returned false" . not . cannotBeTrue