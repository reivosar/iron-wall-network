module Application.BankAccount.UseCases.WithdrawFundsUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.FundsWithdrawn as FundsWithdrawn
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    withdrawAmount :: Double,
    withdrawnAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        FundsWithdrawn.FundsWithdrawn
          { FundsWithdrawn.accountId = accountId input,
            FundsWithdrawn.amount = withdrawAmount input,
            FundsWithdrawn.withdrawnAt = withdrawnAt input
          }

  result <-
    publishEvent
      (accountId input)
      "account"
      "FundsWithdrawn"
      "system"
      event
      Nothing

  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
