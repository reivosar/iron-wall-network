{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Domain.BankAccount.Events.AccountPended as AccountPended
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
import Data.Aeson (toJSON)
import Infrastructure.Events.RedisDomainEventPublisher (publishEvent) 
import Data.UUID (UUID)

-- Create Account Handler
handleCreateAccount ::CreateAccount.CreateAccount -> IO UUID
handleCreateAccount cmd = do
    currentTime <- getCurrentTime
    newAccountId <- nextRandom
    let eventData = AccountCreated.AccountCreated
            { AccountCreated.accountId = newAccountId
            , AccountCreated.username = CreateAccount.username cmd
            , AccountCreated.fullName = CreateAccount.fullName cmd
            , AccountCreated.email = CreateAccount.email cmd
            , AccountCreated.createdAt = currentTime
            }
    publishEvent newAccountId "account" "AccountCreated" "system" eventData Nothing
    return newAccountId

-- Approve Account Handler
handleApproveAccount :: ApproveAccount.ApproveAccount -> IO ()
handleApproveAccount cmd = do
    currentTime <- getCurrentTime
    let accountId = ApproveAccount.accountId cmd
    let eventData = AccountApproved.AccountApproved
            { AccountApproved.accountId = accountId
            , AccountApproved.approvedAt = currentTime
            , AccountApproved.approvalNotes = ApproveAccount.approvalNotes cmd
            }
    publishEvent accountId "account" "AccountApproved" "system" eventData Nothing

-- Submit Pending Account Handler
handleSubmitPendingAccount :: SubmitAccountForApproval.SubmitAccountForApproval -> IO ()
handleSubmitPendingAccount cmd = do
    currentTime <- getCurrentTime
    let accountId = SubmitAccountForApproval.accountId cmd
    let eventData = AccountPended.AccountPended
            { AccountPended.accountId = accountId
            , AccountPended.reason = SubmitAccountForApproval.reason cmd
            , AccountPended.pendedAt = currentTime
            }
    publishEvent accountId "account" "AccountPended" "system" eventData Nothing

-- Suspend Account Handler
handleSuspendAccount :: SuspendAccount.SuspendAccount -> IO ()
handleSuspendAccount cmd = do
    currentTime <- getCurrentTime
    let accountId = SuspendAccount.accountId cmd
    let eventData = AccountSuspended.AccountSuspended
            { AccountSuspended.accountId = accountId
            , AccountSuspended.reason = SuspendAccount.reason cmd
            , AccountSuspended.suspendedAt = currentTime
            }
    publishEvent accountId "account" "AccountSuspended" "system" eventData Nothing

-- Activate Account Handler
handleActivateAccount :: ActivateAccount.ActivateAccount -> IO ()
handleActivateAccount cmd = do
    currentTime <- getCurrentTime
    let accountId = ActivateAccount.accountId cmd
    let eventData = AccountActivated.AccountActivated
            { AccountActivated.accountId = accountId
            , AccountActivated.activatedAt = currentTime
            }
    publishEvent accountId "account" "AccountActivated" "system" eventData Nothing

-- Close Account Handler
handleCloseAccount :: CloseAccount.CloseAccount -> IO ()
handleCloseAccount cmd = do
    currentTime <- getCurrentTime
    let accountId = CloseAccount.accountId cmd
    let eventData = AccountClosed.AccountClosed
            { AccountClosed.accountId = accountId
            , AccountClosed.closedAt = currentTime
            }
    publishEvent accountId "account" "AccountClosed" "system" eventData Nothing

-- Deposit Funds Handler
handleDepositFunds :: DepositFunds.DepositFunds -> IO ()
handleDepositFunds cmd = do
    currentTime <- getCurrentTime
    let accountId = DepositFunds.accountId cmd
    let eventData = FundsDeposited.FundsDeposited
            { FundsDeposited.accountId = accountId
            , FundsDeposited.amount = DepositFunds.depositAmount cmd
            , FundsDeposited.depositedAt = currentTime
            }
    publishEvent accountId "account" "FundsDeposited" "system" eventData Nothing

-- Withdraw Funds Handler
handleWithdrawFunds :: WithdrawFunds.WithdrawFunds -> IO ()
handleWithdrawFunds cmd = do
    currentTime <- getCurrentTime
    let accountId = WithdrawFunds.accountId cmd
    let eventData = FundsWithdrawn.FundsWithdrawn
            { FundsWithdrawn.accountId = accountId
            , FundsWithdrawn.amount = WithdrawFunds.withdrawAmount cmd 
            , FundsWithdrawn.withdrawnAt = currentTime
            }
    publishEvent accountId "account" "FundsWithdrawn" "system" eventData Nothing

-- Upsert User Contact Info Handler
handleUpsertUserContactInfo :: UpsertUserContactInfo.UpsertUserContactInfo -> IO ()
handleUpsertUserContactInfo cmd = do
    currentTime <- getCurrentTime
    let accountId = UpsertUserContactInfo.accountId cmd
    let eventData = UserContactInfoUpserted.UserContactInfoUpserted
            { UserContactInfoUpserted.accountId = accountId
            , UserContactInfoUpserted.email = UpsertUserContactInfo.email cmd
            , UserContactInfoUpserted.updatedAt = currentTime
            }
    publishEvent accountId "account" "UserContactInfoUpserted" "system" eventData Nothing

-- Upsert Phone Number Handler
handleUpsertPhoneNumber :: UpsertPhoneNumber.UpsertPhoneNumber -> IO ()
handleUpsertPhoneNumber cmd = do
    currentTime <- getCurrentTime
    let accountId = UpsertPhoneNumber.accountId cmd
    let eventData = PhoneNumberUpserted.PhoneNumberUpserted
            { PhoneNumberUpserted.accountId = accountId
            , PhoneNumberUpserted.phoneNumber = UpsertPhoneNumber.phoneNumber cmd
            , PhoneNumberUpserted.phoneType = UpsertPhoneNumber.phoneType cmd
            , PhoneNumberUpserted.updatedAt = currentTime
            }
    publishEvent accountId "account" "PhoneNumberUpserted" "system" eventData Nothing

-- Upsert Address Handler
handleUpsertAddress :: UpsertAddress.UpsertAddress -> IO ()
handleUpsertAddress cmd = do
    currentTime <- getCurrentTime
    let accountId = UpsertAddress.accountId cmd
    let eventData = AddressUpserted.AddressUpserted
            { AddressUpserted.accountId = accountId
            , AddressUpserted.address = UpsertAddress.address cmd
            , AddressUpserted.addressType = UpsertAddress.addressType cmd
            , AddressUpserted.updatedAt = currentTime
            }
    publishEvent accountId "account" "AddressUpserted" "system" eventData Nothing

-- Upsert Emergency Contact Handler
handleUpsertEmergencyContact :: UpsertEmergencyContact.UpsertEmergencyContact -> IO ()
handleUpsertEmergencyContact cmd = do
    currentTime <- getCurrentTime
    let accountId = UpsertEmergencyContact.accountId cmd
    let eventData = EmergencyContactUpserted.EmergencyContactUpserted
            { EmergencyContactUpserted.accountId = accountId
            , EmergencyContactUpserted.contactName = UpsertEmergencyContact.contactName cmd
            , EmergencyContactUpserted.contactPhone = UpsertEmergencyContact.contactPhone cmd
            , EmergencyContactUpserted.updatedAt = currentTime
            }
    publishEvent accountId "account" "EmergencyContactUpserted" "system" eventData Nothing