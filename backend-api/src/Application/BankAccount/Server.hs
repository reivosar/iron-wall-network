module Application.BankAccount.Server
  ( activateAccountHandler,
    approveAccountHandler,
    bankAccountServer,
    closeAccountHandler,
    createAccountHandler,
    depositFundsHandler,
    suspendAccountHandler,
    upsertAddressHandler,
    upsertContactInfoHandler,
    upsertEmergencyContactHandler,
    upsertPhoneNumberHandler,
    withdrawFundsHandler,
  )
where

import Application.ApiError (convertApiErrorToHttpError)
import Application.BankAccount.Api (BankApi)
import Application.BankAccount.CommandHandler
import Application.BankAccount.Commands.ActivateAccountCommand as ActivateAccountCommand
import Application.BankAccount.Commands.ApproveAccountCommand as ApproveAccountCommand
import Application.BankAccount.Commands.CloseAccountCommand as CloseAccountCommand
import Application.BankAccount.Commands.CreateAccountCommand as CreateAccountCommand
import Application.BankAccount.Commands.DepositFundsCommand as DepositFundsCommand
import Application.BankAccount.Commands.SuspendAccountCommand as SuspendAccountCommand
import Application.BankAccount.Commands.UpsertAddressCommand as UpsertAddressCommand
import Application.BankAccount.Commands.UpsertEmailContactCommand as UpsertEmailContactCommand
import Application.BankAccount.Commands.UpsertEmergencyContactCommand as UpsertEmergencyContactCommand
import Application.BankAccount.Commands.UpsertPhoneNumberContactCommand as UpsertPhoneNumberContactCommand
import Application.BankAccount.Commands.WithdrawFundsCommand as WithdrawFundsCommand
import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID)
import Servant

bankAccountServer :: Server BankApi
bankAccountServer =
  createAccountHandler
    :<|> approveAccountHandler
    :<|> depositFundsHandler
    :<|> withdrawFundsHandler
    :<|> suspendAccountHandler
    :<|> activateAccountHandler
    :<|> closeAccountHandler
    :<|> upsertContactInfoHandler
    :<|> upsertPhoneNumberHandler
    :<|> upsertAddressHandler
    :<|> upsertEmergencyContactHandler

createAccountHandler :: CreateAccountCommand.CreateAccountCommand -> Handler UUID
createAccountHandler cmd = do
  result <- liftIO $ handleCreateAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right newAccountId -> return newAccountId

approveAccountHandler :: ApproveAccountCommand.ApproveAccountCommand -> Handler NoContent
approveAccountHandler cmd = do
  result <- liftIO $ handleApproveAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

depositFundsHandler :: DepositFundsCommand.DepositFundsCommand -> Handler NoContent
depositFundsHandler cmd = do
  result <- liftIO $ handleDepositFunds cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

withdrawFundsHandler :: WithdrawFundsCommand.WithdrawFundsCommand -> Handler NoContent
withdrawFundsHandler cmd = do
  result <- liftIO $ handleWithdrawFunds cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

suspendAccountHandler :: SuspendAccountCommand.SuspendAccountCommand -> Handler NoContent
suspendAccountHandler cmd = do
  result <- liftIO $ handleSuspendAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

activateAccountHandler :: ActivateAccountCommand.ActivateAccountCommand -> Handler NoContent
activateAccountHandler cmd = do
  result <- liftIO $ handleActivateAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

closeAccountHandler :: CloseAccountCommand.CloseAccountCommand -> Handler NoContent
closeAccountHandler cmd = do
  result <- liftIO $ handleCloseAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertContactInfoHandler :: UpsertEmailContactCommand.UpsertEmailContactCommand -> Handler NoContent
upsertContactInfoHandler cmd = do
  result <- liftIO $ handleUpsertContact cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertPhoneNumberHandler :: UpsertPhoneNumberContactCommand.UpsertPhoneNumberContactCommand -> Handler NoContent
upsertPhoneNumberHandler cmd = do
  result <- liftIO $ handleUpsertPhoneNumber cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertAddressHandler :: UpsertAddressCommand.UpsertAddressCommand -> Handler NoContent
upsertAddressHandler cmd = do
  result <- liftIO $ handleUpsertAddress cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertEmergencyContactHandler :: UpsertEmergencyContactCommand.UpsertEmergencyContactCommand -> Handler NoContent
upsertEmergencyContactHandler cmd = do
  result <- liftIO $ handleUpsertEmergencyContact cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent
