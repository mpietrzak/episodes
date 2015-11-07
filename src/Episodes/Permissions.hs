
module Episodes.Permissions (
    canEditShow,
    canSubmitShowChanges
) where


import Prelude hiding (Show, show)
import Data.Maybe (fromMaybe)

import Model (
    Account,
    AccountId,
    Show,
    showAddedBy,
    showLocal,
    showPublic,
    showSubmitted )


canEditShow :: AccountId -> Show -> Bool
canEditShow accountId show = not (showPublic show) && not (showSubmitted show) && Just accountId == showAddedBy show


canSubmitShowChanges :: AccountId -> Account -> Show -> Bool
canSubmitShowChanges _accountId _account _show =
    not (canEditShow _accountId _show)
    -- && fromMaybe False (showLocal _show)
    && showPublic _show
