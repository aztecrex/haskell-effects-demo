module Spec.Effects (tests) where
---

import Effects (dispatchEMailMessage, dispatchSMSMessage, EMail(..), SMS(..), notifyInteresting, notifyEmergency )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad (void)
import Control.Monad.Freer (run, reinterpret, Eff)
import Control.Monad.Freer.Writer (Writer (..), runWriter, tell)
import Data.Function ((&))
import Data.Text (Text)

tests :: TestTree
tests = testGroup "Effects" [

    testCase "sends an email message" $ do
        let
        -- given
            message = "something happened"

        -- when
            actual = notifyInteresting message

        -- then
        snd (runFriendly actual) @?= [message], -- sends a single email message

    testCase "sends text and email messages" $ do
        let
        -- given
            message = "something bad happened!"

        -- when
            actual = notifyEmergency message

        -- then
        snd (runRude actual) @?= [message, message, message] -- sends 3 text messages
        (snd . fst) (runRude actual) @?= [message] -- sends a single email message

        ]


{-
Pure interpretations for testing. Notice that runFriendly cannot
run programs that require the "SMS" effect.

Effects are great for testing because we can supply any interpretations
we need to support the tests: stubs, fakes, or mocks. Multiple
interpretations for the same effect can be used to support mulpiple
testing scenarios if needed


-}
runFriendly ::  Eff '[EMail] a -> (a, [Text])
runFriendly effs = handleEmail effs & run

runRude :: Eff '[EMail, SMS] a -> ((a, [Text]), [Text])
runRude effs = handleEmail effs & handleSMS & run

{-

Pure handlers for the effects needed by the programs being tested.
They are similar implementations for these test cases: record each
sent message in a list and return list alongside the program result.

-}
handleEmail :: Eff (EMail ': effects) a -> Eff effects (a, [Text])
handleEmail effs = runWriter $ reinterpret impl effs
    where
        impl :: EMail b -> Eff (Writer [Text] ': effects) b
        impl (DispatchEMailMessage msg) = tell [msg]

handleSMS :: Eff (SMS ': effects) a -> Eff effects (a, [Text])
handleSMS effs = runWriter $ reinterpret impl effs
    where
        impl :: SMS b -> Eff (Writer [Text] ': effects) b
        impl (DispatchSMSMessage msg) = tell [msg]
