{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Application.BankAccount.UseCases.UpsertPhoneNumberContactUseCaseSpec (spec) where

import Application.BankAccount.UseCases.UpsertPhoneNumberContactUseCase
import Application.UseCaseError (unwrapUseCaseError)
import Control.Exception (SomeException, toException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON, toJSON)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID, nil)
import Domain.BankAccount.Entity.PhoneNumberContact (PhoneNumberContact, mkPhoneNumberContact)
import qualified Domain.BankAccount.Events.PhoneNumberContactUpserted as PhoneNumberContactUpserted
import Domain.BankAccount.Repositories.PhoneNumberRepository
import Domain.BankAccount.ValueObject.AccountId (AccountId, mkAccountId)
import Domain.BankAccount.ValueObject.PhoneNumber (mkPhoneNumber)
import Domain.BankAccount.ValueObject.PhoneType (textToPhoneType)
import Domain.DomainEventPublisher
import Test.Hspec

-- Mock Environment
data MockEnv = MockEnv
  { mockFindById :: AccountId -> IO (Either SomeException (Maybe PhoneNumberContact)),
    mockSave :: PhoneNumberContact -> IO (Either SomeException ()),
    mockPublishEvent :: UUID -> Text -> Text -> Text -> PhoneNumberContactUpserted.PhoneNumberContactUpserted -> Maybe Value -> IO (Either DomainEventError ())
  }

-- Mock Implementation of PhoneNumberRepository
instance PhoneNumberRepository (ReaderT MockEnv IO) where
  findById accId = do
    env <- ask
    liftIO $ mockFindById env accId
  save phoneNumberContact = do
    env <- ask
    liftIO $ mockSave env phoneNumberContact

-- Mock Implementation of DomainEventPublisher
instance DomainEventPublisher (ReaderT MockEnv IO) where
  publishEvent aggregateId aggregateType eventType triggeredBy eventData metadata = do
    env <- ask
    case castEvent eventData of
      Just event -> liftIO $ mockPublishEvent env aggregateId aggregateType eventType triggeredBy event metadata
      Nothing -> error "Event type mismatch in mockPublishEvent"

-- Helper function to cast event data
castEvent :: forall a. (ToJSON a) => a -> Maybe PhoneNumberContactUpserted.PhoneNumberContactUpserted
castEvent event =
  case fromJSON (toJSON event) of
    Success typedEvent -> Just typedEvent
    _ -> Nothing

spec :: Spec
spec = do
  describe "execute" $ do
    it "should successfully upsert a phone number contact" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                phoneNumber = "090-1234-5678",
                phoneType = "mobile",
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

    it "should return an error if PhoneNumber is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                phoneNumber = "invalid-phone",
                phoneType = "mobile",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid PhoneNumber: Invalid phone number format."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if PhoneType is invalid" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                phoneNumber = "090-1234-5678",
                phoneType = "unknown-type",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Invalid PhoneType: Invalid PhoneType. Expected 'mobile', 'home', or 'work'."
        Right _ -> expectationFailure "Expected an error but got success"

    it "should successfully update an existing phone number contact" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let existingPhoneNumberContact =
            mkPhoneNumberContact
              (mkAccountId nil)
              (fromRight (error "Invalid PhoneType") $ textToPhoneType "mobile")
              (fromRight (error "Invalid PhoneNumber") $ mkPhoneNumber "080-8765-4321")
      let input =
            Input
              { accountId = nil,
                phoneNumber = "090-1234-5678",
                phoneType = "home",
                updatedAt = currentTime
              }
      let mockEnv =
            MockEnv
              { mockFindById = \_ -> return $ Right (Just existingPhoneNumberContact),
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
                phoneNumber = "090-1234-5678",
                phoneType = "mobile",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to fetch phoneNumber: user error (Database error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if save fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                phoneNumber = "090-1234-5678",
                phoneType = "mobile",
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
        Left err -> unwrapUseCaseError err `shouldBe` "Failed to save PhoneNumber: user error (Save error)"
        Right _ -> expectationFailure "Expected an error but got success"

    it "should return an error if publishEvent fails" $ do
      -- GIVEN
      currentTime <- liftIO getCurrentTime
      let input =
            Input
              { accountId = nil,
                phoneNumber = "090-1234-5678",
                phoneType = "mobile",
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
