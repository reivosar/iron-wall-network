{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreApproveAccountFactorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.ApproveAccount
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.Error (DomainError, mkDomainError, unwrapDomainError)
import Infrastructure.Factories.BankAccount.EventStoreApproveAccountFactory
import Test.Hspec

-- Mock environment for testing
data MockEnv = MockEnv
  { mockTryApprove :: AccountId -> IO (Either DomainError ())
  }

instance BankAccountService (ReaderT MockEnv IO) where
  tryCreate _ = return $ Left $ mkDomainError "tryCreate not implemented in MockEnv"
  tryApprove accntId = do
    env <- ask
    liftIO $ mockTryApprove env accntId
  tryActivate _ = return $ Left $ mkDomainError "tryActivate not implemented in MockEnv"
  tryPend _ = return $ Left $ mkDomainError "tryPend not implemented in MockEnv"
  trySuspend _ = return $ Left $ mkDomainError "trySuspend not implemented in MockEnv"
  tryClose _ = return $ Left $ mkDomainError "tryClose not implemented in MockEnv"

spec :: Spec
spec = do
  describe "createApproveAccount" $ do
    it "should create an ApproveAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let givenNotes = Just "Approved by manager"
      let mockEnv =
            MockEnv
              { mockTryApprove = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createApproveAccount uuid currentTime givenNotes) mockEnv

      -- THEN
      let accntId = mkAccountId uuid
      let expectedAccount = mkApproveAccount accntId currentTime givenNotes
      result `shouldBe` Right expectedAccount

    it "should handle missing approval notes" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let givenNotes = Nothing
      let mockEnv =
            MockEnv
              { mockTryApprove = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createApproveAccount uuid currentTime givenNotes) mockEnv

      -- THEN
      let accntId = mkAccountId uuid
      let expectedAccount = mkApproveAccount accntId currentTime givenNotes
      result `shouldBe` Right expectedAccount

    it "should return an error if tryApprove fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let givenNotes = Just "Approval attempt failed"
      let mockEnv =
            MockEnv
              { mockTryApprove = \_ -> return $ Left $ mkDomainError "Approval not allowed"
              }

      -- WHEN
      result <- runReaderT (createApproveAccount uuid currentTime givenNotes) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Approval not allowed"
        Right _ -> expectationFailure "Expected an error, but got a valid ApproveAccount"
