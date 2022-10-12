module Test.Parsel.Single
  ( testTree,
  )
where

import Control.Applicative (many)

import Data.Text (Text)
import Data.Text qualified as Text

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Core (Property, TestTree, property, testGroup, testProp)

--------------------------------------------------------------------------------

import Text.Parsel

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Single"
    [ testProp "char c" propSingle 
    , testProp "many (char c)" propManySingle
    ]

propSingle :: Property
propSingle = property do
  chr <- forAll Gen.unicode
  let source :: Text
      source = Text.singleton chr
   in parse source (char chr) === Right chr

propManySingle :: Property
propManySingle = property do
  chr <- forAll Gen.unicode
  len <- forAll (Gen.int $ Range.linear 0 100)
  let source :: Text
      source = Text.replicate len (Text.pack [chr])
   in parse source (many (char chr)) === Right (replicate len chr)