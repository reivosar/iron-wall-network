{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.DepositFundsUseCaseSpec (spec) where

import Application.BankAccount.UseCases.DepositFundsUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Exception (SomeException, toException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, nil)
import Domain.BankAccount.Entity.Funds (Funds, mkFunds)
import qualified Domain.BankAccount.Events.FundsDeposited as FundsDeposited
import Domain.BankAccount.Repositories.FundsRepository
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.Balance (mkBalance)
import Domain.DomainEventPublisher
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockFindById :: AccountId -> IO (Either SomeException (Maybe Funds)),
    mockSave :: Funds -> IO (Either SomeException ()),
    mockPublishEvent :: UUID -> Text -> Text -> Text -> FundsDeposited.FundsDeposited -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of FundsRepository
instance FundsRepository (ReaderT MockEnv IO) where
  findById accId = do
    env <- ask
    liftIO $ mockFindById env accId
  save funds = do
    env <- ask
    liftIO $ mockSave env funds

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe FundsDeposited.FundsDeposited
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully deposit funds" $ do
      -- GIVEN
      let uuid = nil
      let inputDepositAmount = 50.0
      currentDepositedAt <- liftIO getCurrentTime
      let accountIdVo = mkAccountId uuid
      let funds = fromRight (error "Failed to create mock Funds") (mkFunds accountIdVo (fromRight (error "Balance creation failed") (mkBalance 100.0)))
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just funds),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, depositAmount = inputDepositAmount, depositedAt = currentDepositedAt}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if funds not found" $ do
      -- GIVEN
      currentDepositedAt <- liftIO getCurrentTime
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = nil, depositAmount = 50.0, depositedAt = currentDepositedAt}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Funds not found"
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if fetching funds fails" $ do
      -- GIVEN
      currentDepositedAt <- liftIO getCurrentTime
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Left (toException (userError "Database error")),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = nil, depositAmount = 50.0, depositedAt = currentDepositedAt}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to fetch funds: user error (Database error)"
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if adding balance fails" $ do
      -- GIVEN
      let uuid = nil
      let inputDepositAmount = -50.0 -- Invalid deposit amount
      currentDepositedAt <- liftIO getCurrentTime
      let accountIdVo = mkAccountId uuid
      let funds = fromRight (error "Failed to create mock Funds") (mkFunds accountIdVo (fromRight (error "Balance creation failed") (mkBalance 100.0)))
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just funds),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      let input = Input {accountId = uuid, depositAmount = inputDepositAmount, depositedAt = currentDepositedAt}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Cannot add a negative amount to balance."
        Right _ -> expectationFailure "Expected an error but got a success"

    it "should return an error if event publishing fails" $ do
      -- GIVEN
      let uuid = nil
      let inputDepositAmount = 50.0
      currentDepositedAt <- liftIO getCurrentTime
      let accountIdVo = mkAccountId uuid
      let funds = fromRight (error "Failed to create mock Funds") (mkFunds accountIdVo (fromRight (error "Balance creation failed") (mkBalance 100.0)))
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just funds),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Event publishing failed")
              }
      let input = Input {accountId = uuid, depositAmount = inputDepositAmount, depositedAt = currentDepositedAt}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Event publishing failed"
        Right _ -> expectationFailure "Expected an error but got a success"
