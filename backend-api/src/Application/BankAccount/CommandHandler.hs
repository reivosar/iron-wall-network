module Application.BankAccount.CommandHandler where

import Application.ApiError (ApiError, convertUseCaseErrorToApiError)
import qualified Application.BankAccount.Commands.ActivateAccount as ActivateAccount
import qualified Application.BankAccount.Commands.ApproveAccount as ApproveAccount
import qualified Application.BankAccount.Commands.CloseAccount as CloseAccount
import qualified Application.BankAccount.Commands.CreateAccount as CreateAccount
import qualified Application.BankAccount.Commands.DepositFunds as DepositFunds
import qualified Application.BankAccount.Commands.SubmitAccountForApproval as SubmitAccountForApproval
import qualified Application.BankAccount.Commands.SuspendAccount as SuspendAccount
import qualified Application.BankAccount.Commands.UpsertAddress as UpsertAddress
import qualified Application.BankAccount.Commands.UpsertEmergencyContact as UpsertEmergencyContact
import qualified Application.BankAccount.Commands.UpsertPhoneNumber as UpsertPhoneNumber
import qualified Application.BankAccount.Commands.UpsertUserContactInfo as UpsertUserContactInfo
import qualified Application.BankAccount.Commands.WithdrawFunds as WithdrawFunds
import qualified Application.BankAccount.UseCases.ActivateAccountUseCase as ActivateAccountUseCase
import qualified Application.BankAccount.UseCases.ApproveAccountUseCase as ApproveAccountUseCase
import qualified Application.BankAccount.UseCases.CloseAccountUseCase as CloseAccountUseCase
import qualified Application.BankAccount.UseCases.CreateAccountUseCase as CreateAccountUseCase
import qualified Application.BankAccount.UseCases.DepositFundsUseCase as DepositFundsUseCase
import qualified Application.BankAccount.UseCases.SubmitAccountForApprovalUseCase as SubmitAccountForApprovalUseCase
import qualified Application.BankAccount.UseCases.SuspendAccountUseCase as SuspendAccountUseCase
import qualified Application.BankAccount.UseCases.UpsertAddressUseCase as UpsertAddressUseCase
import qualified Application.BankAccount.UseCases.UpsertEmergencyContactUseCase as UpsertEmergencyContactUseCase
import qualified Application.BankAccount.UseCases.UpsertPhoneNumberUseCase as UpsertPhoneNumberUseCase
import qualified Application.BankAccount.UseCases.UpsertUserContactInfoUseCase as UpsertUserContactInfoUseCase
import qualified Application.BankAccount.UseCases.WithdrawFundsUseCase as WithdrawFundsUseCase
import Data.Bifunctor (first)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)

-- Create Account Handler
handleCreateAccount :: CreateAccount.CreateAccount -> IO (Either ApiError UUID)
handleCreateAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        CreateAccountUseCase.Input
          { CreateAccountUseCase.username = CreateAccount.username cmd,
            CreateAccountUseCase.fullName = CreateAccount.fullName cmd,
            CreateAccountUseCase.email = CreateAccount.email cmd,
            CreateAccountUseCase.createdAt = currentTime
          }
  result <- CreateAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Approve Account Handler
handleApproveAccount :: ApproveAccount.ApproveAccount -> IO (Either ApiError ())
handleApproveAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        ApproveAccountUseCase.Input
          { ApproveAccountUseCase.accountId = ApproveAccount.accountId cmd,
            ApproveAccountUseCase.approvedAt = currentTime,
            ApproveAccountUseCase.approvalNotes = ApproveAccount.approvalNotes cmd
          }
  result <- ApproveAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Submit Pending Account Handler
handleSubmitPendingAccount :: SubmitAccountForApproval.SubmitAccountForApproval -> IO (Either ApiError ())
handleSubmitPendingAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        SubmitAccountForApprovalUseCase.Input
          { SubmitAccountForApprovalUseCase.accountId = SubmitAccountForApproval.accountId cmd,
            SubmitAccountForApprovalUseCase.reason = SubmitAccountForApproval.reason cmd,
            SubmitAccountForApprovalUseCase.pendedAt = currentTime
          }
  result <- SubmitAccountForApprovalUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Suspend Account Handler
handleSuspendAccount :: SuspendAccount.SuspendAccount -> IO (Either ApiError ())
handleSuspendAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        SuspendAccountUseCase.Input
          { SuspendAccountUseCase.accountId = SuspendAccount.accountId cmd,
            SuspendAccountUseCase.reason = SuspendAccount.reason cmd,
            SuspendAccountUseCase.suspendedAt = currentTime
          }
  result <- SuspendAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Activate Account Handler
handleActivateAccount :: ActivateAccount.ActivateAccount -> IO (Either ApiError ())
handleActivateAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        ActivateAccountUseCase.Input
          { ActivateAccountUseCase.accountId = ActivateAccount.accountId cmd,
            ActivateAccountUseCase.activatedAt = currentTime
          }
  result <- ActivateAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Close Account Handler
handleCloseAccount :: CloseAccount.CloseAccount -> IO (Either ApiError ())
handleCloseAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        CloseAccountUseCase.Input
          { CloseAccountUseCase.accountId = CloseAccount.accountId cmd,
            CloseAccountUseCase.closedAt = currentTime
          }
  result <- CloseAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Deposit Funds Handler
handleDepositFunds :: DepositFunds.DepositFunds -> IO (Either ApiError ())
handleDepositFunds cmd = do
  currentTime <- getCurrentTime
  let input =
        DepositFundsUseCase.Input
          { DepositFundsUseCase.accountId = DepositFunds.accountId cmd,
            DepositFundsUseCase.depositAmount = DepositFunds.depositAmount cmd,
            DepositFundsUseCase.depositedAt = currentTime
          }
  result <- DepositFundsUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Withdraw Funds Handler
handleWithdrawFunds :: WithdrawFunds.WithdrawFunds -> IO (Either ApiError ())
handleWithdrawFunds cmd = do
  currentTime <- getCurrentTime
  let input =
        WithdrawFundsUseCase.Input
          { WithdrawFundsUseCase.accountId = WithdrawFunds.accountId cmd,
            WithdrawFundsUseCase.withdrawAmount = WithdrawFunds.withdrawAmount cmd,
            WithdrawFundsUseCase.withdrawnAt = currentTime
          }
  result <- WithdrawFundsUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert User Contact Info Handler
handleUpsertUserContactInfo :: UpsertUserContactInfo.UpsertUserContactInfo -> IO (Either ApiError ())
handleUpsertUserContactInfo cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertUserContactInfoUseCase.Input
          { UpsertUserContactInfoUseCase.accountId = UpsertUserContactInfo.accountId cmd,
            UpsertUserContactInfoUseCase.email = UpsertUserContactInfo.email cmd,
            UpsertUserContactInfoUseCase.updatedAt = currentTime
          }
  result <- UpsertUserContactInfoUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert Phone Number Handler
handleUpsertPhoneNumber :: UpsertPhoneNumber.UpsertPhoneNumber -> IO (Either ApiError ())
handleUpsertPhoneNumber cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertPhoneNumberUseCase.Input
          { UpsertPhoneNumberUseCase.accountId = UpsertPhoneNumber.accountId cmd,
            UpsertPhoneNumberUseCase.phoneNumber = UpsertPhoneNumber.phoneNumber cmd,
            UpsertPhoneNumberUseCase.phoneType = UpsertPhoneNumber.phoneType cmd,
            UpsertPhoneNumberUseCase.updatedAt = currentTime
          }
  result <- UpsertPhoneNumberUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert Address Handler
handleUpsertAddress :: UpsertAddress.UpsertAddress -> IO (Either ApiError ())
handleUpsertAddress cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertAddressUseCase.Input
          { UpsertAddressUseCase.accountId = UpsertAddress.accountId cmd,
            UpsertAddressUseCase.address = UpsertAddress.address cmd,
            UpsertAddressUseCase.addressType = UpsertAddress.addressType cmd,
            UpsertAddressUseCase.updatedAt = currentTime
          }
  result <- UpsertAddressUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert Emergency Contact Handler
handleUpsertEmergencyContact :: UpsertEmergencyContact.UpsertEmergencyContact -> IO (Either ApiError ())
handleUpsertEmergencyContact cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertEmergencyContactUseCase.Input
          { UpsertEmergencyContactUseCase.accountId = UpsertEmergencyContact.accountId cmd,
            UpsertEmergencyContactUseCase.contactName = UpsertEmergencyContact.contactName cmd,
            UpsertEmergencyContactUseCase.contactPhone = UpsertEmergencyContact.contactPhone cmd,
            UpsertEmergencyContactUseCase.updatedAt = currentTime
          }
  result <- UpsertEmergencyContactUseCase.execute input
  return $ first convertUseCaseErrorToApiError result
