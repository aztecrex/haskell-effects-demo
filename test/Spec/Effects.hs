module Spec.Effects (tests) where
---

import Effects

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Freer (run, reinterpret, Eff)
import Control.Monad.Freer.Writer (Writer (..), runWriter, tell)
import Data.Function ((&))
import Data.Text (Text)

tests :: TestTree
tests = testGroup "Effects" [

    testCase "sends a text message" $ do
        let
        -- given
            message = "something happened"

        -- when
            actual = notifyInteresting message

        -- then
        snd (runFriendly actual) @?= [message]

        ]





runFriendly ::  Eff '[EMail] a -> (a, [Text])
runFriendly effs = handleEmail effs & run
-- runFriendly effs = run $ runWriter $ reinterpret impl effs
--     where
--         impl :: EMail b -> Eff (Writer [Text] ': effects) (b, [Text])
--         impl (DispatchEmailMessage msg) = tell [msg]

handleEmail :: Eff (EMail ': effects) a -> Eff effects (a, [Text])
handleEmail effs = runWriter $ reinterpret impl effs
    where
        impl :: EMail b -> Eff (Writer [Text] ': effects) b
        impl (DispatchEmailMessage msg) = tell [msg]
