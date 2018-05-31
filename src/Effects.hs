module Effects where
---

import Control.Monad (void)
import Control.Monad.Freer (send, Eff, Member, Members, LastMember, interpretM, runM)
import Data.Function ((&))
import Data.Text (Text)
import qualified Network.AWS.SNS.Publish as SQS
import qualified Network.AWS as AWS
import Control.Lens


{-

Here are the effectful programs we deliver. Both are under test
coverage.  The first program depends only on the "EMail" effect.
That is, it can run in any environment that supplies an
interpratation of that one effect.

The second places more restrictions on the environment. I depends
on both "EMail" and "SMS."  It can run in any environment that
supplies an interpretation of both but it cannot run in an
environment that is missing an interepration for either of those
effects.

-}
notifyInteresting :: (Member EMail effects) => Text -> Eff effects ()
notifyInteresting = dispatchEMailMessage

notifyEmergency :: (Members '[EMail, SMS] effects) => Text -> Eff effects ()
notifyEmergency msg = do
    dispatchEMailMessage msg
    dispatchSMSMessage msg
    dispatchSMSMessage msg
    dispatchSMSMessage msg


{-
Effect languages. These are used by the notification programs. They don't
imply any particular interpretations. The effect model of "Control.
Monad.Freer" is one of an _authority_ to which programs send requests.
Thus effects are modeled as data i.e. the effect request to be sent

The "EMail" effect, for instance provides the constructor
"DispatchEMailMessage" which takes a message as text and returns nothing
(unit  return). The "EMail" effect could supply more operations by
defining them as additional constructors.

The "dispatchEmailFunction" is simply a convenience that sends a constructed
request to the authority. Again, the authority is an abstract effect
performer. Implementations are not implied by these definitions.

-}
data EMail a where
    DispatchEMailMessage :: Text -> EMail ()

dispatchEMailMessage :: (Member EMail effects) => Text -> Eff effects ()
dispatchEMailMessage msg = send $ DispatchEMailMessage msg

data SMS a where
    DispatchSMSMessage :: Text -> SMS ()

dispatchSMSMessage :: (Member SMS effects) => Text -> Eff effects ()
dispatchSMSMessage msg = send $ DispatchSMSMessage msg


{-

Effect handlers. In order to run an effectful program, a handler for
each required effect is supplied by the environment.

These handlers both use AWS Simple Notification Service to interpret
their respective effects. In their declarations, notice that each takes
a stack of effects and pops off the type it handles. Popping off all
the effects determines tot total interpration of an effecftul program.

-}
handleEMailWithAWS :: (LastMember IO effects) => Eff (EMail ': effects) a -> Eff effects a
handleEMailWithAWS effs = interpretM impl effs
    where
        impl :: EMail a -> IO a
        impl (DispatchEMailMessage msg) = sns topic msg
        topic :: Text
        topic = "arn:aws:sns:us-east-1:845290112482:deleteme-gjw-email-effects-demo"

handleSMSWithAWS :: (LastMember IO effects) => Eff (SMS ': effects) a -> Eff effects a
handleSMSWithAWS effs = interpretM impl effs
    where
        impl :: SMS a -> IO a
        impl (DispatchSMSMessage msg) = sns topic msg
        topic :: Text
        topic = "arn:aws:sns:us-east-1:845290112482:deleteme-gjw-sms-effects-demo"



{-
Effectful environments. These cobine effect handlers as needed to interpret
a program. The first supplies an interpretation of EMail only.
Thus it can run "notifyInteresting" but it cannot run "notifyEmergency."

The second can run either of the effectful programs because it supplies
interpretations of all the necessary effects for both (even though not all
of them are used in all programs).

-}
runFriendlyNotifications :: Eff '[EMail, IO] a -> IO a
runFriendlyNotifications effs = handleEMailWithAWS effs & runM

runRudeNotifications :: Eff '[EMail, SMS, IO] a -> IO a
runRudeNotifications effs = handleEMailWithAWS effs & handleSMSWithAWS & runM



{-
AWS SNS implementation used by both handlers
-}
sns :: Text -> Text -> IO ()
sns topic message = do
    let req = SQS.publish message & SQS.pTopicARN .~ Just topic
    env <- AWS.newEnv AWS.Discover
    void $ AWS.runResourceT . AWS.runAWS env $ AWS.send req

