module Main (main) where

import Test.Tasty (defaultMain)

--------------------------------------------------------------------------------

import Test.Compat (TestTree, testGroup)
import Test.Parsel qualified 

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Parsel.testTree
    ]