{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.CreateAccountUseCaseSpec (spec) where

import Application.BankAccount.Factories.BankAccountFactory
import Application.BankAccount.UseCases.CreateAccountUseCase
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.InitialAccount (InitialAccount, mkInitialAccount)
import qualified Domain.BankAccount.Events.AccountCreated as AccountCreated
import Domain.BankAccount.Repositories.AccountRepository
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)
import Domain.DomainEventPublisher
import Domain.Error (DomainError)
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockCreateBankAccount :: Text -> Text -> Text -> UTCTime -> IO (Either DomainError InitialAccount),
    mockPublishEvent :: UUID.UUID -> Text -> Text -> Text -> AccountCreated.AccountCreated -> Maybe Value -> IO (Either DomainEventError ()),
    mockGenerateAccountId :: IO AccountId
  }

-- Mock Implementation of BankAccountFactory
instance {-# OVERLAPPING #-} BankAccountFactory (ReaderT MockEnv IO) where
  createBankAccount inputUsername inputFullName inputEmail inputCreatedAt = do
    env <- ask
    liftIO $ mockCreateBankAccount env inputUsername inputFullName inputEmail inputCreatedAt

-- Mock Implementation of AccountRepository
instance {-# OVERLAPPING #-} AccountRepository (ReaderT MockEnv IO) where
  generateAccountId = do
    env <- ask
    liftIO $ mockGenerateAccountId env

-- Mock Implementation of DomainEventPublisher
instance {-# OVERLAPPING #-} DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe AccountCreated.AccountCreated
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully create an account" $ do
      -- GIVEN
      let testUUID = UUID.nil
      let testUsername = "testuser"
      let testFullName = "Test User"
      let testEmail = "test@example.com"
      testCreatedAt <- liftIO getCurrentTime
      let initialAccount =
            mkInitialAccount
              (mkAccountId testUUID)
              (either (error "Invalid username") id $ mkUsername testUsername)
              (either (error "Invalid full name") id $ mkFullName testFullName)
              (either (error "Invalid email") id $ mkEmail testEmail)
              testCreatedAt
      let mockEnv =
            MockEnv
              { mockCreateBankAccount = \_ _ _ _ -> return $ Right initialAccount,
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right (),
                mockGenerateAccountId = return $ mkAccountId UUID.nil
              }
      let input = Input {username = testUsername, fullName = testFullName, email = testEmail, createdAt = testCreatedAt}

      -- WHEN
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right accountId -> accountId `shouldBe` testUUID
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err
