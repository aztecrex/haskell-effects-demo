module Effects where
---

import Control.Monad.Freer (send, Eff, Member)
import Data.Text (Text)

notifyInteresting :: (Member EMail effects) => Text -> Eff effects ()
notifyInteresting = dispatchEmailMessage

data EMail a where
    DispatchEmailMessage :: Text -> EMail ()

dispatchEmailMessage :: (Member EMail effects) => Text -> Eff effects ()
dispatchEmailMessage msg = send $ DispatchEmailMessage msg

