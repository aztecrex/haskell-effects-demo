module Effects where
---

import Control.Monad (void)
import Control.Monad.Freer (send, Eff, Member, Members, LastMember, interpretM, runM)
import Data.Function ((&))
import Data.Text (Text)
import qualified Network.AWS.SNS.Publish as SQS
import qualified Network.AWS as AWS
import Control.Lens

notifyInteresting :: (Member EMail effects) => Text -> Eff effects ()
notifyInteresting = dispatchEMailMessage

notifyEmergency :: (Members '[EMail, SMS] effects) => Text -> Eff effects ()
notifyEmergency msg = do
    dispatchEMailMessage msg
    dispatchSMSMessage msg
    dispatchSMSMessage msg
    dispatchSMSMessage msg

runNotifications :: Eff '[EMail, SMS, IO] a -> IO a
runNotifications effs = handleEMailWithAWS effs & handleSMSWithAWS & runM

data EMail a where
    DispatchEMailMessage :: Text -> EMail ()

dispatchEMailMessage :: (Member EMail effects) => Text -> Eff effects ()
dispatchEMailMessage msg = send $ DispatchEMailMessage msg

data SMS a where
    DispatchSMSMessage :: Text -> SMS ()

dispatchSMSMessage :: (Member SMS effects) => Text -> Eff effects ()
dispatchSMSMessage msg = send $ DispatchSMSMessage msg


handleEMailWithAWS :: (LastMember IO effects) => Eff (EMail ': effects) a -> Eff effects a
handleEMailWithAWS effs = interpretM impl effs
    where
        impl :: EMail a -> IO a
        impl (DispatchEMailMessage msg) = sms topic msg
        topic :: Text
        topic = "arn:aws:sns:us-east-1:845290112482:deleteme-gjw-email-effects-demo"

handleSMSWithAWS :: (LastMember IO effects) => Eff (SMS ': effects) a -> Eff effects a
handleSMSWithAWS effs = interpretM impl effs
    where
        impl :: SMS a -> IO a
        impl (DispatchSMSMessage msg) = sms topic msg
        topic :: Text
        topic = "arn:aws:sns:us-east-1:845290112482:deleteme-gjw-sms-effects-demo"

sms :: Text -> Text -> IO ()
sms topic message = do
    let req = SQS.publish message & SQS.pTopicARN .~ Just topic
    env <- AWS.newEnv AWS.Discover
    void $ AWS.runResourceT . AWS.runAWS env $ AWS.send req

