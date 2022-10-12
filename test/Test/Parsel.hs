
module Test.Parsel 
  ( testTree,
  )
where 

import Test.Compat (TestTree, testGroup)

--------------------------------------------------------------------------------

import Test.Parsel.Single qualified 

--------------------------------------------------------------------------------

testTree :: TestTree
testTree = 
  testGroup
    "Parsel"
    [ Test.Parsel.Single.testTree
    ] 