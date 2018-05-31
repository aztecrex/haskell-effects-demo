module Spec.Transformers (tests) where
---

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Transformers" [
        testCase "wired" $ True @?= True
    ]
