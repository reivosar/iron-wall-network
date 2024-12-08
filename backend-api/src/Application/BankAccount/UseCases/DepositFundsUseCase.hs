{-# LANGUAGE FlexibleContexts #-}

module Application.BankAccount.UseCases.DepositFundsUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.Funds (Funds, addBalance, depositFunds)
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import Domain.BankAccount.Repositories.FundsRepository (FundsRepository, findById, save)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.ValueError (ValueError (..))
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    depositAmount :: Double,
    depositedAt :: UTCTime
  }

execute :: (FundsRepository m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  case mkAccountId (accountId input) of
    Left err -> return $ Left (createValidationError "Invalid Account ID format")
    Right accId -> do
      fundsResult <- findById accId
      case fundsResult of
        Left err -> return $ Left (createSystemError $ "Failed to fetch funds: " ++ show err)
        Right Nothing -> return $ Left (createValidationError "Funds not found")
        Right (Just funds) -> do
          case addBalance funds (depositAmount input) of
            Left (ValueError msg) -> return $ Left (createValidationError msg)
            Right updatedFunds -> do
              let event = depositFunds updatedFunds (depositAmount input) (depositedAt input)
              eventResult <-
                liftIO $
                  publishEvent
                    (FundsDeposited.accountId event)
                    "account"
                    "FundsDeposited"
                    "system"
                    event
                    Nothing
              case eventResult of
                Left (RedisConnectionError msg) ->
                  return $ Left (createSystemError ("Redis connection error: " ++ show msg))
                Left (RedisCommandError msg) ->
                  return $ Left (createSystemError ("Redis command error: " ++ show msg))
                Left (EventStoreError msg) ->
                  return $ Left (createValidationError ("Failed to store event: " ++ show msg))
                Right _ -> return $ Right ()
