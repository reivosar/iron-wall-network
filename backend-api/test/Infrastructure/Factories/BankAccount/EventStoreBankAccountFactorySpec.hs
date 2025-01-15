{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreBankAccountFactorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.InitialAccount
import Domain.BankAccount.Repositories.AccountRepository
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username
import Domain.Error (DomainError, mkDomainError, unwrapDomainError)
import Infrastructure.Factories.BankAccount.EventStoreBankAccountFactory
import Test.Hspec

-- Mock environment for testing
data MockEnv = MockEnv
  { mockGenerateAccountId :: IO (AccountId),
    mockTryCreate :: Username -> IO (Either DomainError ())
  }

-- Mock AccountRepository instance
instance AccountRepository (ReaderT MockEnv IO) where
  generateAccountId = do
    env <- ask
    liftIO $ mockGenerateAccountId env

-- Mock BankAccountService instance
instance BankAccountService (ReaderT MockEnv IO) where
  tryCreate usrNm = do
    env <- ask
    liftIO $ mockTryCreate env usrNm
  tryApprove _ = return $ Left $ mkDomainError "tryApprove not implemented in MockEnv"
  tryActivate _ = return $ Left $ mkDomainError "tryActivate not implemented in MockEnv"
  tryPend _ = return $ Left $ mkDomainError "tryPend not implemented in MockEnv"
  trySuspend _ = return $ Left $ mkDomainError "trySuspend not implemented in MockEnv"
  tryClose _ = return $ Left $ mkDomainError "tryClose not implemented in MockEnv"

spec :: Spec
spec = do
  describe "createBankAccount" $ do
    it "should create a BankAccount with valid inputs" $ do
      -- GIVEN
      currentTime <- getCurrentTime
      let testUsername = "validUsername"
      let testFullName = "Valid Full Name"
      let testEmail = "valid.email@example.com"
      let mockEnv =
            MockEnv
              { mockGenerateAccountId = return $ mkAccountId UUID.nil,
                mockTryCreate = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createBankAccount testUsername testFullName testEmail currentTime) mockEnv

      -- THEN
      case result of
        Right bankAccount -> do
          let expectedAccountId = mkAccountId UUID.nil
          let expectedUsername = either (error "Failed to create Username") id (mkUsername testUsername)
          let expectedFullName = either (error "Failed to create FullName") id (mkFullName testFullName)
          let expectedEmail = either (error "Failed to create Email") id (mkEmail testEmail)
          let expectedBankAccount = mkInitialAccount expectedAccountId expectedUsername expectedFullName expectedEmail currentTime
          bankAccount `shouldBe` expectedBankAccount
        Left err -> expectationFailure $ "Expected a valid BankAccount, but got error: " <> unpack (unwrapDomainError err)

    it "should return an error if tryCreate fails" $ do
      -- GIVEN
      currentTime <- getCurrentTime
      let testUsername = "validUsername"
      let testFullName = "Valid Full Name"
      let testEmail = "valid.email@example.com"
      let mockEnv =
            MockEnv
              { mockGenerateAccountId = return $ mkAccountId UUID.nil,
                mockTryCreate = \_ -> return $ Left $ mkDomainError "Account creation not allowed"
              }

      -- WHEN
      result <- runReaderT (createBankAccount testUsername testFullName testEmail currentTime) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Account creation not allowed"
        Right _ -> expectationFailure "Expected an error, but got a valid BankAccount"

    it "should return an error for invalid email" $ do
      -- GIVEN
      currentTime <- getCurrentTime
      let testUsername = "validUsername"
      let testFullName = "Valid Full Name"
      let invalidEmail = "invalid-email"
      let mockEnv =
            MockEnv
              { mockGenerateAccountId = return $ mkAccountId UUID.nil,
                mockTryCreate = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createBankAccount testUsername testFullName invalidEmail currentTime) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Invalid email format."
        Right _ -> expectationFailure "Expected an error for invalid email, but got a valid BankAccount"
