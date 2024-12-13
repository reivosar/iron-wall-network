{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.IO.Class
  ( MonadIO,
  )
import Data.Text ()
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Domain.BankAccount.Entity.Funds
  ( addBalance,
    depositFunds,
  )
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import Domain.BankAccount.Repositories.FundsRepository
  ( FundsRepository,
    findById,
  )
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.DomainEventPublisher
import Domain.ValueError (ValueError (..))

data Input = Input
  { accountId :: UUID,
    depositAmount :: Double,
    depositedAt :: UTCTime
  }

execute :: (FundsRepository m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  case mkAccountId (accountId input) of
    Left _ -> return $ Left (createValidationError "Invalid Account ID format")
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
                publishEvent
                  (FundsDeposited.accountId event)
                  "account"
                  "FundsDeposited"
                  "system"
                  event
                  Nothing
              case eventResult of
                Left err -> return $ Left (mapDomainEventErrorToUseCaseError err)
                Right _ -> return $ Right ()
