{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Episodes.DB.Shows (searchShows) where

import Prelude
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist (Entity, toPersistValue)
import Database.Persist.Sql (SqlPersistT, rawSql)
import Text.Shakespeare.Text (st)
import qualified Data.Text as T

import qualified Model as M


searchShowsForUser :: MonadIO m => M.AccountId -> Maybe Text -> SqlPersistT m [(Entity M.Show, Bool)]
searchShowsForUser accountId query = fmap _fixrow <$> rawSql _sql _params
    where
        _fixrow :: (Entity M.Show, Maybe M.SubscriptionId) -> (Entity M.Show, Bool)
        _fixrow (e, mid) = (e, isJust mid)
        _sql = [st|
                select ??, subscription.id
                from show
                left join
                    (select * from subscription where account = ?) as subscription
                    on (subscription.show = show.id)
                where
                    title ilike ? escape '_'
                    and (
                        show.public
                        or show.added_by = ?
                    )
            |]
        _params = [ toPersistValue accountId
                  , toPersistValue _pattern
                  , toPersistValue accountId ]
        _pattern = case query of
            Just _query -> "%" <> T.replace "%" "_%" _query <> "%"
            Nothing -> "%"


searchShowsForAnon :: MonadIO m => Maybe Text -> SqlPersistT m [(Entity M.Show, Bool)]
searchShowsForAnon query = fmap (\x -> (x, False)) <$> rawSql _sql _params
    where
        _sql = [st|
                select ??
                from show
                where
                    title ilike ? escape '_'
                    and show.public
            |]
        _params = [ toPersistValue _pattern ]
        _pattern = case query of
            Just _query -> "%" <> T.replace "%" "_%" _query <> "%"
            Nothing -> "%"


searchShows :: MonadIO m => Maybe M.AccountId -> Maybe Text -> SqlPersistT m [(Entity M.Show, Bool)]
searchShows mAccountId query = case mAccountId of
    Just _a -> searchShowsForUser _a query
    Nothing -> searchShowsForAnon query

