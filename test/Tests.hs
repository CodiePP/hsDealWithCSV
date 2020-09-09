
module Main
  (
    main
  ) where

import Test.Tasty
import qualified DwCSV.Tests.Basic as Basic

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Deal with CSV"
  [
    Basic.tests
  ]
