module Domain.BankAccount.Entity.ApproveAccount
  ( ApproveAccount (..),
    mkApproveAccount,
    accountApproved,
  )
where

import Data.Text (Text)
import Data.Time.Clock
  ( UTCTime,
    getCurrentTime,
  )
import qualified Domain.BankAccount.Events.AccountApproved as Event
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    unwrapAccountId,
  )

data ApproveAccount = ApproveAccount
  { accountId :: AccountId,
    approvedAt :: UTCTime,
    approvalNotes :: Maybe Text
  }
  deriving (Show, Eq)

mkApproveAccount :: AccountId -> UTCTime -> Maybe Text -> ApproveAccount
mkApproveAccount accId apprvdAt notes =
  ApproveAccount
    { accountId = accId,
      approvedAt = apprvdAt,
      approvalNotes = notes
    }

accountApproved :: ApproveAccount -> Event.AccountApproved
accountApproved approveAcc =
  Event.AccountApproved
    { Event.accountId = unwrapAccountId (accountId approveAcc),
      Event.approvedAt = approvedAt approveAcc,
      Event.approvalNotes = approvalNotes approveAcc
    }
