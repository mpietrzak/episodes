
module Episodes.DB.Account (
    createAccount
) where


import Prelude
import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime)
import Data.Text (Text)
import Database.Persist (Key, insertUnique)
import Database.Persist.Sql (SqlPersistT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TLE
import qualified Data.Text.Encoding.Error as TLEE

import Episodes.Utils (hashPassword)
import qualified Model as M


-- createAccount :: MonadIO m
--               => Text -> Maybe Text -> UTCTime -> SqlPersistT m (Key Account)
-- createAccount usernameOrEmail mPassword now = do
--     let (_username, _email) = if T.any ((==) '@') usernameOrEmail
--             then (Nothing, Just usernameOrEmail)
--             else (Just usernameOrEmail, Nothing)
--     let acc = Account { accountNickname = _username
--                       , accountEmail = _email
--                       , accountPassword = mPassword
--                       , accountAdmin = False
--                       , accountAccessedApprox = now
--                       , accountViews = 0
--                       , accountCreated = now
--                       , accountModified = now }
--     accId <- insert acc
--     return accId


-- | Try to insert user account and return True if insert was successful.
createAccount :: MonadIO m => UTCTime -> Text -> Maybe Text -> SqlPersistT m (Maybe (Key M.Account))
createAccount now username mPassword = do
    let nickname = if (T.any ('@' ==) username)
            then Nothing
            else Just username
    let email = if T.any ('@' ==) username
            then Just username
            else Nothing
    let mPassHash = case mPassword of
            Just password -> Just passHash
                where
                    salt = "xxxx" -- TODO
                    passHash = TLE.decodeUtf8With (TLEE.replace '?') $ hashPassword salt $ TLE.encodeUtf8 password
            _ -> Nothing
    let acc = M.Account { M.accountNickname = nickname
                        , M.accountEmail = email
                        , M.accountPassword = mPassHash
                        , M.accountAdmin = False
                        , M.accountAccessedApprox = now
                        , M.accountViews = 0
                        , M.accountCreated = now
                        , M.accountModified = now }
    insertUnique acc
