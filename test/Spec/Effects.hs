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
        snd (runFriendly actual) @?= [message],

    testCase "sends a text message" $ do
        let
        -- given
            message = "something bad happened!"

        -- when
            actual = notifyEmergency message

        -- then
        snd (runRude actual) @?= [message, message, message]
        (snd . fst) (runRude actual) @?= [message]

        ]



runFriendly ::  Eff '[EMail] a -> (a, [Text])
runFriendly effs = handleEmail effs & run

runRude :: Eff '[EMail, SMS] a -> ((a, [Text]), [Text])
runRude effs = handleEmail effs & handleSMS & run

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
