module Application.BankAccount.Server where

import Application.Api (BankApi)
import Application.ApiError (convertApiErrorToHttpError)
import Application.BankAccount.CommandHandler
import Application.BankAccount.Commands.ActivateAccount as ActivateAccount
import Application.BankAccount.Commands.ApproveAccount as ApproveAccount
import Application.BankAccount.Commands.CloseAccount as CloseAccount
import Application.BankAccount.Commands.CreateAccount as CreateAccount
import Application.BankAccount.Commands.DepositFunds as DepositFunds
import Application.BankAccount.Commands.SuspendAccount as SuspendAccount
import Application.BankAccount.Commands.UpsertAddress as UpsertAddress
import Application.BankAccount.Commands.UpsertEmergencyContact as UpsertEmergencyContact
import Application.BankAccount.Commands.UpsertPhoneNumber as UpsertPhoneNumber
import Application.BankAccount.Commands.UpsertUserContactInfo as UpsertUserContactInfo
import Application.BankAccount.Commands.WithdrawFunds as WithdrawFunds
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

createAccountHandler :: CreateAccount.CreateAccount -> Handler UUID
createAccountHandler cmd = do
  result <- liftIO $ handleCreateAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right accountId -> return accountId

approveAccountHandler :: ApproveAccount.ApproveAccount -> Handler NoContent
approveAccountHandler cmd = do
  result <- liftIO $ handleApproveAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

depositFundsHandler :: DepositFunds.DepositFunds -> Handler NoContent
depositFundsHandler cmd = do
  result <- liftIO $ handleDepositFunds cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

withdrawFundsHandler :: WithdrawFunds.WithdrawFunds -> Handler NoContent
withdrawFundsHandler cmd = do
  result <- liftIO $ handleWithdrawFunds cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

suspendAccountHandler :: SuspendAccount.SuspendAccount -> Handler NoContent
suspendAccountHandler cmd = do
  result <- liftIO $ handleSuspendAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

activateAccountHandler :: ActivateAccount.ActivateAccount -> Handler NoContent
activateAccountHandler cmd = do
  result <- liftIO $ handleActivateAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

closeAccountHandler :: CloseAccount.CloseAccount -> Handler NoContent
closeAccountHandler cmd = do
  result <- liftIO $ handleCloseAccount cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertContactInfoHandler :: UpsertUserContactInfo.UpsertUserContactInfo -> Handler NoContent
upsertContactInfoHandler cmd = do
  result <- liftIO $ handleUpsertUserContactInfo cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertPhoneNumberHandler :: UpsertPhoneNumber.UpsertPhoneNumber -> Handler NoContent
upsertPhoneNumberHandler cmd = do
  result <- liftIO $ handleUpsertPhoneNumber cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertAddressHandler :: UpsertAddress.UpsertAddress -> Handler NoContent
upsertAddressHandler cmd = do
  result <- liftIO $ handleUpsertAddress cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent

upsertEmergencyContactHandler :: UpsertEmergencyContact.UpsertEmergencyContact -> Handler NoContent
upsertEmergencyContactHandler cmd = do
  result <- liftIO $ handleUpsertEmergencyContact cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent
