{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.UpsertEmailContactUseCaseSpec (spec) where

import Application.BankAccount.UseCases.UpsertEmailContactUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Exception (SomeException, toException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, nil)
import Domain.BankAccount.Entity.EmailContact (EmailContact, mkEmailContact)
import qualified Domain.BankAccount.Events.EmailContactUpserted as EmailContactUpserted
import Domain.BankAccount.Repositories.EmailContactRepository
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.DomainEventPublisher
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockFindById :: AccountId -> IO (Either SomeException (Maybe EmailContact)),
    mockSave :: EmailContact -> IO (Either SomeException ()),
    mockPublishEvent :: UUID -> Text -> Text -> Text -> EmailContactUpserted.EmailContactUpserted -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of EmailContactRepository
instance EmailContactRepository (ReaderT MockEnv IO) where
  findById accId = do
    env <- ask
    liftIO $ mockFindById env accId
  save emailContact = do
    env <- ask
    liftIO $ mockSave env emailContact

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe EmailContactUpserted.EmailContactUpserted
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully upsert an email contact" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                email = "test@example.com",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if email is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                email = "invalid-email",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid Email: Invalid email format."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should successfully update an existing email contact" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let existingEmailContact =
            mkEmailContact
              (mkAccountId nil)
              (fromRight (error "Invalid Email") $ mkEmail "old@example.com")
      let input =
            Input
              { accountId = nil,
                email = "new@example.com",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just existingEmailContact),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Expected success, but got error: " <> show err

    it "should return an error if findById fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                email = "test@example.com",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Left (toException $ userError "Database error"),
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to fetch EmailContact: user error (Database error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if save fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                email = "test@example.com",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Left (toException $ userError "Save error"),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Right ()
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to save EmailContact: user error (Save error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if publishEvent fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                email = "test@example.com",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right Nothing,
                mockSave = \_ -> return $ Right (),
                mockPublishEvent = \_ _ _ _ _ _ -> return $ Left (PublishEventFailed "Publish error")
              }
      result <- runReaderT (execute input) mockEnv

      -- THEN
      case result of
        Left err -> unwrapUseCaseError err `shouldBe` "Event publishing failed: Publish error"
        Right _ -> expectationFailure "Expected an error but got success"
