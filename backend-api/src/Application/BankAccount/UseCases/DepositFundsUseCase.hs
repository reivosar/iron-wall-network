{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.UseCases.DepositFundsUseCase
  ( Input (..),
    execute,
  )
where

import Application.UseCaseError
  ( UseCaseError,
    createSystemError,
    createValidationError,
    mapDomainEventErrorToUseCaseError,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.AggregateType (AggregateType (..), aggregateTypeToText)
import Domain.BankAccount.Entity.Funds
  ( Funds,
    addBalance,
    depositFunds,
  )
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import Domain.BankAccount.Repositories.FundsRepository (FundsRepository, findById)
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.DomainEventPublisher
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    depositAmount :: Double,
    depositedAt :: UTCTime
  }

execute ::
  (FundsRepository m, DomainEventPublisher m, MonadIO m) =>
  Input ->
  m (Either UseCaseError ())
execute input = do
  let accId = mkAccountId (accountId input)
  findFunds accId >>= processFunds input

findFunds :: (FundsRepository m, Monad m) => AccountId -> m (Either UseCaseError (Maybe Funds))
findFunds accId = do
  result <- findById accId
  return $ case result of
    Left err -> Left $ createSystemError $ "Failed to fetch funds: " <> pack (show err)
    Right funds -> Right funds

processFunds ::
  (DomainEventPublisher m, MonadIO m) =>
  Input ->
  Either UseCaseError (Maybe Funds) ->
  m (Either UseCaseError ())
processFunds _ (Left err) = return $ Left err
processFunds _ (Right Nothing) = return $ Left $ createValidationError "Funds not found"
processFunds input (Right (Just funds)) = updateFundsAndPublishEvent input funds

updateFundsAndPublishEvent ::
  (DomainEventPublisher m, MonadIO m) =>
  Input ->
  Funds ->
  m (Either UseCaseError ())
updateFundsAndPublishEvent input funds = do
  case addBalance funds (depositAmount input) of
    Left err -> return $ Left $ createValidationError $ unwrapValueError err
    Right updatedFunds -> publishFundsDepositedEvent input updatedFunds

publishFundsDepositedEvent ::
  (DomainEventPublisher m, Monad m) =>
  Input ->
  Funds ->
  m (Either UseCaseError ())
publishFundsDepositedEvent input updatedFunds = do
  let event = depositFunds updatedFunds (depositAmount input) (depositedAt input)

  result <-
    publishEvent
      (FundsDeposited.accountId event)
      (aggregateTypeToText Account)
      FundsDeposited.eventName
      "system"
      event
      Nothing

  return $ case result of
    Left err -> Left $ mapDomainEventErrorToUseCaseError err
    Right _ -> Right ()
