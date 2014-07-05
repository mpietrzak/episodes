module Handler.Fay where

import Import

import Fay.Convert (readFromFay)

import Yesod.Auth (requireAuthId)
import Yesod.Fay


-- Action to create show subscription for currently logged in user.
-- I have no idea what this type means though.
subscribeShow :: Integer -> HandlerT App IO ()
subscribeShow showId = do
    userId <- requireAuthId
    _ <- runDB $ insert_ $ Subscription { subscriptionUser = userId, subscriptionShow = Key $ PersistInt64 $ fromInteger showId }
    return ()


onCommand :: CommandHandler App
onCommand render command =
    case readFromFay command of
      Just (SubscribeShow showId r) -> render r =<< subscribeShow showId
      Nothing                       -> invalidArgs ["Invalid command"]

