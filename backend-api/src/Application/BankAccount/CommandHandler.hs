module Application.BankAccount.CommandHandler
  ( handleActivateAccount,
    handleApproveAccount,
    handleCloseAccount,
    handleCreateAccount,
    handleDepositFunds,
    handleSuspendAccount,
    handleUpsertAddress,
    handleUpsertEmergencyContact,
    handleUpsertPhoneNumber,
    handleUpsertContact,
    handleWithdrawFunds,
  )
where

import Application.ApiError
  ( ApiError,
    convertUseCaseErrorToApiError,
  )
import qualified Application.BankAccount.Commands.ActivateAccountCommand as ActivateAccountCommand
import qualified Application.BankAccount.Commands.ApproveAccountCommand as ApproveAccountCommand
import qualified Application.BankAccount.Commands.CloseAccountCommand as CloseAccountCommand
import qualified Application.BankAccount.Commands.CreateAccountCommand as CreateAccountCommand
import qualified Application.BankAccount.Commands.DepositFundsCommand as DepositFundsCommand
import qualified Application.BankAccount.Commands.SuspendAccountCommand as SuspendAccountCommand
import qualified Application.BankAccount.Commands.UpsertAddressCommand as UpsertAddressCommand
import qualified Application.BankAccount.Commands.UpsertEmailContactCommand as UpsertEmailContactCommand
import qualified Application.BankAccount.Commands.UpsertEmergencyContactCommand as UpsertEmergencyContactCommand
import qualified Application.BankAccount.Commands.UpsertPhoneNumberContactCommand as UpsertPhoneNumberContactCommand
import qualified Application.BankAccount.Commands.WithdrawFundsCommand as WithdrawFundsCommand
import qualified Application.BankAccount.UseCases.ActivateAccountUseCase as ActivateAccountUseCase
import qualified Application.BankAccount.UseCases.ApproveAccountUseCase as ApproveAccountUseCase
import qualified Application.BankAccount.UseCases.CloseAccountUseCase as CloseAccountUseCase
import qualified Application.BankAccount.UseCases.CreateAccountUseCase as CreateAccountUseCase
import qualified Application.BankAccount.UseCases.DepositFundsUseCase as DepositFundsUseCase
import qualified Application.BankAccount.UseCases.SuspendAccountUseCase as SuspendAccountUseCase
import qualified Application.BankAccount.UseCases.UpsertAddressUseCase as UpsertAddressUseCase
import qualified Application.BankAccount.UseCases.UpsertEmailContactUseCase as UpsertEmailContactUseCase
import qualified Application.BankAccount.UseCases.UpsertEmergencyContactUseCase as UpsertEmergencyContactUseCase
import qualified Application.BankAccount.UseCases.UpsertPhoneNumberContactUseCase as UpsertPhoneNumberContactUseCase
import qualified Application.BankAccount.UseCases.WithdrawFundsUseCase as WithdrawFundsUseCase
import Data.Bifunctor (first)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import Infrastructure.Events.PostgresDomainEventStore
import Infrastructure.Events.RedisDomainEventPublisher
import Infrastructure.Factories.BankAccount.EventStoreActiveAccountFactory
import Infrastructure.Factories.BankAccount.EventStoreApproveAccountFactory
import Infrastructure.Factories.BankAccount.EventStoreBankAccountFactory
import Infrastructure.Factories.BankAccount.EventStoreCloseAccountFactory
import Infrastructure.Factories.BankAccount.EventStorePendingAccountFactory
import Infrastructure.Factories.BankAccount.EventStoreSuspendAccountFactory
import Infrastructure.Repositories.BankAccount.EventStoreAddressRepository
import Infrastructure.Repositories.BankAccount.EventStoreEmailContactRepository
import Infrastructure.Repositories.BankAccount.EventStoreEmergencyContactRepository
import Infrastructure.Repositories.BankAccount.EventStoreFundsRepository
import Infrastructure.Repositories.BankAccount.EventStorePhoneNumberRepository
import Infrastructure.Services.BankAccount.EventStoreBankAccountService
import Infrastructure.Services.BankAccount.RedisUsernameUniquenessValidator

-- Create Account Handler
handleCreateAccount :: CreateAccountCommand.CreateAccountCommand -> IO (Either ApiError UUID)
handleCreateAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        CreateAccountUseCase.Input
          { CreateAccountUseCase.username = CreateAccountCommand.username cmd,
            CreateAccountUseCase.fullName = CreateAccountCommand.fullName cmd,
            CreateAccountUseCase.email = CreateAccountCommand.email cmd,
            CreateAccountUseCase.createdAt = currentTime
          }
  result <- CreateAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Approve Account Handler
handleApproveAccount :: ApproveAccountCommand.ApproveAccountCommand -> IO (Either ApiError ())
handleApproveAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        ApproveAccountUseCase.Input
          { ApproveAccountUseCase.accountId = ApproveAccountCommand.accountId cmd,
            ApproveAccountUseCase.approvedAt = currentTime,
            ApproveAccountUseCase.approvalNotes = ApproveAccountCommand.approvalNotes cmd
          }
  result <- ApproveAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Suspend Account Handler
handleSuspendAccount :: SuspendAccountCommand.SuspendAccountCommand -> IO (Either ApiError ())
handleSuspendAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        SuspendAccountUseCase.Input
          { SuspendAccountUseCase.accountId = SuspendAccountCommand.accountId cmd,
            SuspendAccountUseCase.reason = SuspendAccountCommand.reason cmd,
            SuspendAccountUseCase.suspendedAt = currentTime
          }
  result <- SuspendAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Activate Account Handler
handleActivateAccount :: ActivateAccountCommand.ActivateAccountCommand -> IO (Either ApiError ())
handleActivateAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        ActivateAccountUseCase.Input
          { ActivateAccountUseCase.accountId = ActivateAccountCommand.accountId cmd,
            ActivateAccountUseCase.password = ActivateAccountCommand.password cmd,
            ActivateAccountUseCase.activatedAt = currentTime
          }
  result <- ActivateAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Close Account Handler
handleCloseAccount :: CloseAccountCommand.CloseAccountCommand -> IO (Either ApiError ())
handleCloseAccount cmd = do
  currentTime <- getCurrentTime
  let input =
        CloseAccountUseCase.Input
          { CloseAccountUseCase.accountId = CloseAccountCommand.accountId cmd,
            CloseAccountUseCase.reason = CloseAccountCommand.reason cmd,
            CloseAccountUseCase.closedAt = currentTime
          }
  result <- CloseAccountUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Deposit Funds Handler
handleDepositFunds :: DepositFundsCommand.DepositFundsCommand -> IO (Either ApiError ())
handleDepositFunds cmd = do
  currentTime <- getCurrentTime
  let input =
        DepositFundsUseCase.Input
          { DepositFundsUseCase.accountId = DepositFundsCommand.accountId cmd,
            DepositFundsUseCase.depositAmount = DepositFundsCommand.depositAmount cmd,
            DepositFundsUseCase.depositedAt = currentTime
          }
  result <- DepositFundsUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Withdraw Funds Handler
handleWithdrawFunds :: WithdrawFundsCommand.WithdrawFundsCommand -> IO (Either ApiError ())
handleWithdrawFunds cmd = do
  currentTime <- getCurrentTime
  let input =
        WithdrawFundsUseCase.Input
          { WithdrawFundsUseCase.accountId = WithdrawFundsCommand.accountId cmd,
            WithdrawFundsUseCase.withdrawAmount = WithdrawFundsCommand.withdrawAmount cmd,
            WithdrawFundsUseCase.withdrawnAt = currentTime
          }
  result <- WithdrawFundsUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert User Contact Info Handler
handleUpsertContact :: UpsertEmailContactCommand.UpsertEmailContactCommand -> IO (Either ApiError ())
handleUpsertContact cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertEmailContactUseCase.Input
          { UpsertEmailContactUseCase.accountId = UpsertEmailContactCommand.accountId cmd,
            UpsertEmailContactUseCase.email = UpsertEmailContactCommand.email cmd,
            UpsertEmailContactUseCase.updatedAt = currentTime
          }
  result <- UpsertEmailContactUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert Phone Number Handler
handleUpsertPhoneNumber :: UpsertPhoneNumberContactCommand.UpsertPhoneNumberContactCommand -> IO (Either ApiError ())
handleUpsertPhoneNumber cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertPhoneNumberContactUseCase.Input
          { UpsertPhoneNumberContactUseCase.accountId = UpsertPhoneNumberContactCommand.accountId cmd,
            UpsertPhoneNumberContactUseCase.phoneNumber = UpsertPhoneNumberContactCommand.phoneNumber cmd,
            UpsertPhoneNumberContactUseCase.phoneType = UpsertPhoneNumberContactCommand.phoneType cmd,
            UpsertPhoneNumberContactUseCase.updatedAt = currentTime
          }
  result <- UpsertPhoneNumberContactUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert Address Handler
handleUpsertAddress :: UpsertAddressCommand.UpsertAddressCommand -> IO (Either ApiError ())
handleUpsertAddress cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertAddressUseCase.Input
          { UpsertAddressUseCase.accountId = UpsertAddressCommand.accountId cmd,
            UpsertAddressUseCase.postalCode = UpsertAddressCommand.postalCode cmd,
            UpsertAddressUseCase.prefecture = UpsertAddressCommand.prefecture cmd,
            UpsertAddressUseCase.city = UpsertAddressCommand.city cmd,
            UpsertAddressUseCase.townArea = UpsertAddressCommand.townArea cmd,
            UpsertAddressUseCase.buildingName = UpsertAddressCommand.buildingName cmd,
            UpsertAddressUseCase.addressType = UpsertAddressCommand.addressType cmd,
            UpsertAddressUseCase.updatedAt = currentTime
          }
  result <- UpsertAddressUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Upsert Emergency Contact Handler
handleUpsertEmergencyContact :: UpsertEmergencyContactCommand.UpsertEmergencyContactCommand -> IO (Either ApiError ())
handleUpsertEmergencyContact cmd = do
  currentTime <- getCurrentTime
  let input =
        UpsertEmergencyContactUseCase.Input
          { UpsertEmergencyContactUseCase.accountId = UpsertEmergencyContactCommand.accountId cmd,
            UpsertEmergencyContactUseCase.contactName = UpsertEmergencyContactCommand.contactName cmd,
            UpsertEmergencyContactUseCase.contactPhone = UpsertEmergencyContactCommand.contactPhone cmd,
            UpsertEmergencyContactUseCase.updatedAt = currentTime
          }
  result <- UpsertEmergencyContactUseCase.execute input
  return $ first convertUseCaseErrorToApiError result
