
module Test.Core
  ( TestTree,
    Property,
    property,
    testGroup,
    testProp,
  )
where

import Hedgehog (Property, property)
import Test.Tasty (TestTree, testGroup)

import Test.Compat (testProp)

--------------------------------------------------------------------------------