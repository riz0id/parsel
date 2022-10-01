
module Test.Core
  ( TestTree,
    testGroup,
    testProp,
  )
where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)

import Test.Compat (testProp)

--------------------------------------------------------------------------------