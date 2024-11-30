module Application.BankAccount.UseCases.DepositFundsUseCase where

import Application.UseCaseError (UseCaseError, createSystemError, createValidationError)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import Infrastructure.Events.RedisDomainEventPublisher (DomainEventPublisherError (..), publishEvent)

data Input = Input
  { accountId :: UUID,
    depositAmount :: Double,
    depositedAt :: UTCTime
  }

execute :: Input -> IO (Either UseCaseError ())
execute input = do
  let event =
        FundsDeposited.FundsDeposited
          { FundsDeposited.accountId = accountId input,
            FundsDeposited.amount = depositAmount input,
            FundsDeposited.depositedAt = depositedAt input
          }
  result <- publishEvent (accountId input) "account" "FundsDeposited" "system" event Nothing
  case result of
    Left (RedisConnectionError msg) ->
      return $ Left (createSystemError ("Redis connection error: " ++ show msg))
    Left (RedisCommandError msg) ->
      return $ Left (createSystemError ("Redis command error: " ++ show msg))
    Left (EventStoreError msg) ->
      return $ Left (createValidationError ("Failed to store event: " ++ show msg))
    Right _ -> return $ Right ()
