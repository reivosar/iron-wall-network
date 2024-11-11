module Application.BankAccount.Server where

import Servant
import Control.Monad.IO.Class (liftIO)
import Application.BankAccount.Commands.CreateAccount as CreateAccount
import Application.BankAccount.Commands.ApproveAccount as ApproveAccount
import Application.BankAccount.Commands.DepositFunds as DepositFunds
import Application.BankAccount.Commands.WithdrawFunds as WithdrawFunds
import Application.BankAccount.Commands.SuspendAccount as SuspendAccount
import Application.BankAccount.Commands.ActivateAccount as ActivateAccount
import Application.BankAccount.Commands.CloseAccount as CloseAccount
import Application.BankAccount.Commands.UpsertUserContactInfo as UpsertUserContactInfo
import Application.BankAccount.Commands.UpsertPhoneNumber as UpsertPhoneNumber
import Application.BankAccount.Commands.UpsertAddress as UpsertAddress
import Application.BankAccount.Commands.UpsertEmergencyContact as UpsertEmergencyContact
import Application.BankAccount.CommandHandler
import Application.Api (BankApi)

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

createAccountHandler :: CreateAccount.CreateAccount -> Handler NoContent
createAccountHandler cmd = do
    _ <- liftIO $ handleCreateAccount cmd
    return NoContent

approveAccountHandler :: ApproveAccount.ApproveAccount -> Handler NoContent
approveAccountHandler cmd = do
    _ <- liftIO $ handleApproveAccount cmd
    return NoContent

depositFundsHandler :: DepositFunds.DepositFunds -> Handler NoContent
depositFundsHandler cmd = do
    _ <- liftIO $ handleDepositFunds cmd
    return NoContent

withdrawFundsHandler :: WithdrawFunds.WithdrawFunds -> Handler NoContent
withdrawFundsHandler cmd = do
    _ <- liftIO $ handleWithdrawFunds cmd
    return NoContent

suspendAccountHandler :: SuspendAccount.SuspendAccount -> Handler NoContent
suspendAccountHandler cmd = do
    _ <- liftIO $ handleSuspendAccount cmd
    return NoContent

activateAccountHandler :: ActivateAccount.ActivateAccount -> Handler NoContent
activateAccountHandler cmd = do
    _ <- liftIO $ handleActivateAccount cmd
    return NoContent

closeAccountHandler :: CloseAccount.CloseAccount -> Handler NoContent
closeAccountHandler cmd = do
    _ <- liftIO $ handleCloseAccount cmd
    return NoContent

upsertContactInfoHandler :: UpsertUserContactInfo.UpsertUserContactInfo -> Handler NoContent
upsertContactInfoHandler cmd = do
    _ <- liftIO $ handleUpsertUserContactInfo cmd
    return NoContent

upsertPhoneNumberHandler :: UpsertPhoneNumber.UpsertPhoneNumber -> Handler NoContent
upsertPhoneNumberHandler cmd = do
    _ <- liftIO $ handleUpsertPhoneNumber cmd
    return NoContent

upsertAddressHandler :: UpsertAddress.UpsertAddress -> Handler NoContent
upsertAddressHandler cmd = do
    _ <- liftIO $ handleUpsertAddress cmd
    return NoContent

upsertEmergencyContactHandler :: UpsertEmergencyContact.UpsertEmergencyContact -> Handler NoContent
upsertEmergencyContactHandler cmd = do
    _ <- liftIO $ handleUpsertEmergencyContact cmd
    return NoContent
