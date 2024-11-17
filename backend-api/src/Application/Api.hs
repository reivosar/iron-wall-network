{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Application.Api where

import Servant
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
import Data.UUID (UUID)

type BankApi =
      "account" :> "create" :> ReqBody '[JSON] CreateAccount.CreateAccount :> Post '[JSON] UUID
  :<|> "account" :> "approve" :> ReqBody '[JSON] ApproveAccount.ApproveAccount :> Post '[JSON] NoContent
  :<|> "account" :> "deposit" :> ReqBody '[JSON] DepositFunds.DepositFunds :> Post '[JSON] NoContent
  :<|> "account" :> "withdraw" :> ReqBody '[JSON] WithdrawFunds.WithdrawFunds :> Post '[JSON] NoContent
  :<|> "account" :> "suspend" :> ReqBody '[JSON] SuspendAccount.SuspendAccount :> Post '[JSON] NoContent
  :<|> "account" :> "activate" :> ReqBody '[JSON] ActivateAccount.ActivateAccount :> Post '[JSON] NoContent
  :<|> "account" :> "close" :> ReqBody '[JSON] CloseAccount.CloseAccount :> Post '[JSON] NoContent
  :<|> "account" :> "contact" :> ReqBody '[JSON] UpsertUserContactInfo.UpsertUserContactInfo :> Post '[JSON] NoContent
  :<|> "account" :> "contact" :> "phone" :> ReqBody '[JSON] UpsertPhoneNumber.UpsertPhoneNumber :> Post '[JSON] NoContent
  :<|> "account" :> "contact" :> "address" :> ReqBody '[JSON] UpsertAddress.UpsertAddress :> Post '[JSON] NoContent
  :<|> "account" :> "contact" :> "emergency" :> ReqBody '[JSON] UpsertEmergencyContact.UpsertEmergencyContact :> Post '[JSON] NoContent
