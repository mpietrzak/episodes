{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Prelude hiding (Show)
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time.Clock
import qualified Prelude


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


-- deriving instance Prelude.Show Episode
-- deriving instance Prelude.Show Profile

