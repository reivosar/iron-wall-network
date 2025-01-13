{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.PostgresBankAccountFactorySpec (spec) where

import Application.BankAccount.Factories.BankAccountFactory
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.InitialAccount
import Domain.BankAccount.Repositories.AccountRepository (AccountRepository, generateAccountId)
import Domain.BankAccount.ValueObject.AccountId (mkAccountId)
import Domain.BankAccount.ValueObject.Email (mkEmail)
import Domain.BankAccount.ValueObject.FullName (mkFullName)
import Domain.BankAccount.ValueObject.Username (mkUsername)
import Domain.ValueError (unwrapValueError)
import Infrastructure.Factories.PostgresBankAccountFactory
import Test.Hspec

-- Mock environment
data MockEnv = MockEnv

-- Mock AccountRepository instance
instance {-# OVERLAPPING #-} AccountRepository (ReaderT MockEnv IO) where
  generateAccountId = return $ mkAccountId UUID.nil

instance {-# OVERLAPPING #-} BankAccountFactory (ReaderT MockEnv IO) where
  createBankAccount usernameTxt fullNameTxt emailTxt createdAtTime = do
    accountIdGenerated <- generateAccountId
    let result = do
          usernameVal <- mkUsername usernameTxt
          fullNameVal <- mkFullName fullNameTxt
          emailVal <- mkEmail emailTxt
          Right $ mkInitialAccount accountIdGenerated usernameVal fullNameVal emailVal createdAtTime
    pure result

spec :: Spec
spec = do
  describe "createBankAccount" $ do
    it "should create a BankAccount with valid inputs" $ do
      -- GIVEN
      currentTime <- getCurrentTime
      let testUsername = "validUsername"
      let testFullName = "Valid Full Name"
      let testEmail = "valid.email@example.com"
      let mockEnv = MockEnv

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
        Left err -> expectationFailure $ "Expected a valid BankAccount, but got error: " <> unpack (unwrapValueError err)

    it "should return an error for invalid email" $ do
      -- GIVEN
      currentTime <- getCurrentTime
      let testUsername = "validUsername"
      let testFullName = "Valid Full Name"
      let invalidEmail = "invalid-email"
      let mockEnv = MockEnv

      -- WHEN
      result <- runReaderT (createBankAccount testUsername testFullName invalidEmail currentTime) mockEnv

      -- THEN
      case result of
        Left err -> unwrapValueError err `shouldBe` "Invalid email format."
        Right _ -> expectationFailure "Expected an error for invalid email, but got a valid BankAccount"
