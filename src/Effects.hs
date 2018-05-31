module Effects where
---

import Control.Monad.Freer (send, Eff, Member, Members)
import Data.Text (Text)

notifyInteresting :: (Member EMail effects) => Text -> Eff effects ()
notifyInteresting = dispatchEmailMessage

notifyEmergency :: (Members '[EMail, SMS] effects) => Text -> Eff effects ()
notifyEmergency msg = do
    dispatchEmailMessage msg
    dispatchSMSMessage msg
    dispatchSMSMessage msg
    dispatchSMSMessage msg

data EMail a where
    DispatchEmailMessage :: Text -> EMail ()

dispatchEmailMessage :: (Member EMail effects) => Text -> Eff effects ()
dispatchEmailMessage msg = send $ DispatchEmailMessage msg

data SMS a where
    DispatchSMSMessage :: Text -> SMS ()

dispatchSMSMessage :: (Member SMS effects) => Text -> Eff effects ()
dispatchSMSMessage msg = send $ DispatchSMSMessage msg
