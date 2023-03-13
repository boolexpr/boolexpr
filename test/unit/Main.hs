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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Boolean Expressions" [
    parseabilityTests
  , matchingTests
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

