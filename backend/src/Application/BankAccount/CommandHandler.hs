{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.BankAccount.CommandHandler where

import qualified Application.BankAccount.Commands.CreateAccount as CreateAccount
import qualified Application.BankAccount.Commands.ApproveAccount as ApproveAccount
import qualified Application.BankAccount.Commands.SubmitAccountForApproval as SubmitAccountForApproval
import qualified Application.BankAccount.Commands.SuspendAccount as SuspendAccount
import qualified Application.BankAccount.Commands.ActivateAccount as ActivateAccount
import qualified Application.BankAccount.Commands.CloseAccount as CloseAccount
import qualified Application.BankAccount.Commands.DepositFunds as DepositFunds
import qualified Application.BankAccount.Commands.WithdrawFunds as WithdrawFunds
import qualified Application.BankAccount.Commands.UpsertUserContactInfo as UpsertUserContactInfo
import qualified Application.BankAccount.Commands.UpsertPhoneNumber as UpsertPhoneNumber
import qualified Application.BankAccount.Commands.UpsertAddress as UpsertAddress
import qualified Application.BankAccount.Commands.UpsertEmergencyContact as UpsertEmergencyContact
import qualified Domain.BankAccount.Events.AccountCreated as AccountCreated
import qualified Domain.BankAccount.Events.AccountApproved as AccountApproved
import qualified Domain.BankAccount.Events.AccountPending as AccountPending
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import qualified Domain.BankAccount.Events.FundsWithdrawn as FundsWithdrawn
import qualified Domain.BankAccount.Events.AccountSuspended as AccountSuspended
import qualified Domain.BankAccount.Events.AccountActivated as AccountActivated
import qualified Domain.BankAccount.Events.AccountClosed as AccountClosed
import qualified Domain.BankAccount.Events.UserContactInfoUpserted as UserContactInfoUpserted
import qualified Domain.BankAccount.Events.PhoneNumberUpserted as PhoneNumberUpserted
import qualified Domain.BankAccount.Events.AddressUpserted as AddressUpserted
import qualified Domain.BankAccount.Events.EmergencyContactUpserted as EmergencyContactUpserted
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom) 

-- Create Account Handler
handleCreateAccount :: CreateAccount.CreateAccount -> IO AccountCreated.AccountCreated
handleCreateAccount cmd = do
    currentTime <- getCurrentTime
    newAccountId <- nextRandom
    return AccountCreated.AccountCreated
      { AccountCreated.accountId = newAccountId
      , AccountCreated.username = CreateAccount.username cmd
      , AccountCreated.fullName = CreateAccount.fullName cmd
      , AccountCreated.email = CreateAccount.email cmd
      , AccountCreated.createdAt = currentTime
      }

-- Approve Account Handler
handleApproveAccount :: ApproveAccount.ApproveAccount -> IO AccountApproved.AccountApproved
handleApproveAccount cmd = do
    currentTime <- getCurrentTime
    return AccountApproved.AccountApproved
      { AccountApproved.accountId = ApproveAccount.accountId cmd
      , AccountApproved.approvedAt = currentTime
      , AccountApproved.approvalNotes = ApproveAccount.approvalNotes cmd
      }

-- Submit Pending Account Handler
handleSubmitPendingAccount :: SubmitAccountForApproval.SubmitAccountForApproval -> IO AccountPending.AccountPending
handleSubmitPendingAccount cmd = do
    currentTime <- getCurrentTime
    return AccountPending.AccountPending
      { AccountPending.accountId = SubmitAccountForApproval.accountId cmd
      , AccountPending.accountHolderName = SubmitAccountForApproval.submitterNotes cmd
      , AccountPending.reason = SubmitAccountForApproval.submitterNotes cmd
      , AccountPending.pendedAt = currentTime
      }

-- Suspend Account Handler
handleSuspendAccount :: SuspendAccount.SuspendAccount -> IO AccountSuspended.AccountSuspended
handleSuspendAccount cmd = do
    currentTime <- getCurrentTime
    return AccountSuspended.AccountSuspended
      { AccountSuspended.accountId = SuspendAccount.accountId cmd
      , AccountSuspended.reason = SuspendAccount.suspendReason cmd
      , AccountSuspended.suspendedAt = currentTime
      }

-- Activate Account Handler
handleActivateAccount :: ActivateAccount.ActivateAccount -> IO AccountActivated.AccountActivated
handleActivateAccount cmd = do
    currentTime <- getCurrentTime
    return AccountActivated.AccountActivated
      { AccountActivated.accountId = ActivateAccount.accountId cmd
      , AccountActivated.activatedAt = currentTime
      }

-- Close Account Handler
handleCloseAccount :: CloseAccount.CloseAccount -> IO AccountClosed.AccountClosed
handleCloseAccount cmd = do
    currentTime <- getCurrentTime
    return AccountClosed.AccountClosed
      { AccountClosed.accountId = CloseAccount.accountId cmd
      , AccountClosed.closedAt = currentTime
      }

-- Deposit Funds Handler
handleDepositFunds :: DepositFunds.DepositFunds -> IO FundsDeposited.FundsDeposited
handleDepositFunds cmd = do
    currentTime <- getCurrentTime
    return FundsDeposited.FundsDeposited
      { FundsDeposited.accountId = DepositFunds.accountId cmd
      , FundsDeposited.amount = DepositFunds.depositAmount cmd
      , FundsDeposited.depositedAt = currentTime
      }

-- Withdraw Funds Handler
handleWithdrawFunds :: WithdrawFunds.WithdrawFunds -> IO FundsWithdrawn.FundsWithdrawn
handleWithdrawFunds cmd = do
    currentTime <- getCurrentTime
    return FundsWithdrawn.FundsWithdrawn
      { FundsWithdrawn.accountId = WithdrawFunds.accountId cmd
      , FundsWithdrawn.amount = WithdrawFunds.withdrawAmount cmd
      , FundsWithdrawn.withdrawnAt = currentTime
      }

-- Upsert User Contact Info Handler
handleUpsertUserContactInfo :: UpsertUserContactInfo.UpsertUserContactInfo -> IO UserContactInfoUpserted.UserContactInfoUpserted
handleUpsertUserContactInfo cmd = do
    currentTime <- getCurrentTime
    return UserContactInfoUpserted.UserContactInfoUpserted
      { UserContactInfoUpserted.accountId = UpsertUserContactInfo.accountId cmd
      , UserContactInfoUpserted.email = UpsertUserContactInfo.email cmd
      , UserContactInfoUpserted.updatedAt = currentTime
      }

-- Upsert Phone Number Handler
handleUpsertPhoneNumber :: UpsertPhoneNumber.UpsertPhoneNumber -> IO PhoneNumberUpserted.PhoneNumberUpserted
handleUpsertPhoneNumber cmd = do
    currentTime <- getCurrentTime
    return PhoneNumberUpserted.PhoneNumberUpserted
      { PhoneNumberUpserted.accountId = UpsertPhoneNumber.accountId cmd
      , PhoneNumberUpserted.phoneNumber = UpsertPhoneNumber.phoneNumber cmd
      , PhoneNumberUpserted.phoneType = UpsertPhoneNumber.phoneType cmd
      , PhoneNumberUpserted.updatedAt = currentTime
      }

-- Upsert Address Handler
handleUpsertAddress :: UpsertAddress.UpsertAddress -> IO AddressUpserted.AddressUpserted
handleUpsertAddress cmd = do
    currentTime <- getCurrentTime
    return AddressUpserted.AddressUpserted
      { AddressUpserted.accountId = UpsertAddress.accountId cmd
      , AddressUpserted.address = UpsertAddress.address cmd
      , AddressUpserted.addressType = UpsertAddress.addressType cmd
      , AddressUpserted.updatedAt = currentTime
      }

-- Upsert Emergency Contact Handler
handleUpsertEmergencyContact :: UpsertEmergencyContact.UpsertEmergencyContact -> IO EmergencyContactUpserted.EmergencyContactUpserted
handleUpsertEmergencyContact cmd = do
    currentTime <- getCurrentTime
    return EmergencyContactUpserted.EmergencyContactUpserted
      { EmergencyContactUpserted.accountId = UpsertEmergencyContact.accountId cmd
      , EmergencyContactUpserted.contactName = UpsertEmergencyContact.contactName cmd
      , EmergencyContactUpserted.contactPhone = UpsertEmergencyContact.contactPhone cmd
      , EmergencyContactUpserted.updatedAt = currentTime
      }
