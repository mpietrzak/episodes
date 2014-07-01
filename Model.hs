{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Prelude hiding (Show)
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import qualified Prelude as Prelude

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

deriving instance Prelude.Show Profile
