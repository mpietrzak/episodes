
module Episodes.Permissions (
    canAcceptChanges,
    canEditShow,
    canSubmitShowChanges
) where


import Prelude hiding (Show, show)
import Data.Maybe (fromMaybe)

import Model (
    Account,
    AccountId,
    Show,
    accountAdmin,
    showAddedBy,
    showLocal,
    showPublic,
    showSubmitted )


canEditShow :: AccountId -> Show -> Bool
canEditShow accountId show = not (showPublic show) && not (showSubmitted show) && Just accountId == showAddedBy show


canSubmitShowChanges :: AccountId -> Account -> Show -> Bool
canSubmitShowChanges _accountId _account _show =
    accountAdmin _account
    && not (canEditShow _accountId _show)
    -- && fromMaybe False (showLocal _show)
    -- && showPublic _show


canAcceptChanges :: Account -> Bool
canAcceptChanges = accountAdmin
