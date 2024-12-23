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
import Control.Monad.IO.Class
  ( MonadIO,
  )
import Data.Text (pack)
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
import Domain.ValueError (unwrapValueError)

data Input = Input
  { accountId :: UUID,
    depositAmount :: Double,
    depositedAt :: UTCTime
  }

execute :: (FundsRepository m, DomainEventPublisher m, MonadIO m) => Input -> m (Either UseCaseError ())
execute input = do
  let accId = mkAccountId (accountId input)
  fundsResult <- findById accId
  case fundsResult of
    Left err -> return $ Left (createSystemError $ "Failed to fetch funds: " <> pack (show err))
    Right Nothing -> return $ Left (createValidationError "Funds not found")
    Right (Just funds) -> do
      case addBalance funds (depositAmount input) of
        Left err -> return $ Left (createValidationError (unwrapValueError err))
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
