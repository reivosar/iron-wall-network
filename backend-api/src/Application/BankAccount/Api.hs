{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application.BankAccount.Api where

import Application.ApiError (convertApiErrorToHttpError)
import Application.BankAccount.Commands.ActivateAccountCommand as ActivateAccountCommand
import Application.BankAccount.Commands.ApproveAccountCommand as ApproveAccountCommand
import Application.BankAccount.Commands.CloseAccountCommand as CloseAccountCommand
import Application.BankAccount.Commands.CreateAccountCommand as CreateAccountCommand
import Application.BankAccount.Commands.DepositFundsCommand as DepositFundsCommand
import Application.BankAccount.Commands.SuspendAccountCommand as SuspendAccountCommand
import Application.BankAccount.Commands.UpsertAddressCommand as UpsertAddressCommand
import Application.BankAccount.Commands.UpsertEmergencyContactCommand as UpsertEmergencyContactCommand
import Application.BankAccount.Commands.UpsertPhoneNumberCommand as UpsertPhoneNumberCommand
import Application.BankAccount.Commands.UpsertUserContactInfoCommand as UpsertUserContactInfoCommand
import Application.BankAccount.Commands.WithdrawFundsCommand as WithdrawFundsCommand
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

type BankApi =
  "account" :> "create" :> ReqBody '[JSON] CreateAccountCommand.CreateAccountCommand :> Post '[JSON] UUID
    :<|> "account" :> "approve" :> ReqBody '[JSON] ApproveAccountCommand.ApproveAccountCommand :> Post '[JSON] NoContent
    :<|> "account" :> "deposit" :> ReqBody '[JSON] DepositFundsCommand.DepositFundsCommand :> Post '[JSON] NoContent
    :<|> "account" :> "withdraw" :> ReqBody '[JSON] WithdrawFundsCommand.WithdrawFundsCommand :> Post '[JSON] NoContent
    :<|> "account" :> "suspend" :> ReqBody '[JSON] SuspendAccountCommand.SuspendAccountCommand :> Post '[JSON] NoContent
    :<|> "account" :> "activate" :> ReqBody '[JSON] ActivateAccountCommand.ActivateAccountCommand :> Post '[JSON] NoContent
    :<|> "account" :> "close" :> ReqBody '[JSON] CloseAccountCommand.CloseAccountCommand :> Post '[JSON] NoContent
    :<|> "account" :> "contact" :> ReqBody '[JSON] UpsertUserContactInfoCommand.UpsertUserContactInfoCommand :> Post '[JSON] NoContent
    :<|> "account" :> "contact" :> "phone" :> ReqBody '[JSON] UpsertPhoneNumberCommand.UpsertPhoneNumberCommand :> Post '[JSON] NoContent
    :<|> "account" :> "contact" :> "address" :> ReqBody '[JSON] UpsertAddressCommand.UpsertAddressCommand :> Post '[JSON] NoContent
    :<|> "account" :> "contact" :> "emergency" :> ReqBody '[JSON] UpsertEmergencyContactCommand.UpsertEmergencyContactCommand :> Post '[JSON] NoContent
