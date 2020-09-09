
module DwCSV.Tests.Basic
  (
    tests
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)


tests :: TestTree
tests = testGroup "testing" [
        property_tests
      ]

property_tests :: TestTree
property_tests = testGroup "Property tests" [
        testProperty "test1: random list" propRandlist
      ]

propRandlist ::  [Int] -> Bool
propRandlist ints = length ints >= 0

