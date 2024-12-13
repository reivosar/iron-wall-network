{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application.BankAccount.Api (BankApi) where

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
import Data.UUID (UUID)
import Servant

type BankApi =
  "v1" :> "account" :> "create" :> ReqBody '[JSON] CreateAccountCommand.CreateAccountCommand :> Post '[JSON] UUID
    :<|> "v1" :> "account" :> "approve" :> ReqBody '[JSON] ApproveAccountCommand.ApproveAccountCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "deposit" :> ReqBody '[JSON] DepositFundsCommand.DepositFundsCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "withdraw" :> ReqBody '[JSON] WithdrawFundsCommand.WithdrawFundsCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "suspend" :> ReqBody '[JSON] SuspendAccountCommand.SuspendAccountCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "activate" :> ReqBody '[JSON] ActivateAccountCommand.ActivateAccountCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "close" :> ReqBody '[JSON] CloseAccountCommand.CloseAccountCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "contact" :> ReqBody '[JSON] UpsertUserContactInfoCommand.UpsertUserContactInfoCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "contact" :> "phone" :> ReqBody '[JSON] UpsertPhoneNumberCommand.UpsertPhoneNumberCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "contact" :> "address" :> ReqBody '[JSON] UpsertAddressCommand.UpsertAddressCommand :> Post '[JSON] NoContent
    :<|> "v1" :> "account" :> "contact" :> "emergency" :> ReqBody '[JSON] UpsertEmergencyContactCommand.UpsertEmergencyContactCommand :> Post '[JSON] NoContent
