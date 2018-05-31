module Main where

import Test.Tasty (testGroup, TestTree, defaultMain)
import qualified Spec.Effects as Effects (tests)
import qualified Spec.Transformers as Transformers (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Haskell Effects" [
        Effects.tests,
        Transformers.tests
    ]
