{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStorePendingAccountFactorySpec (spec) where

import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.PendingAccount
import Domain.BankAccount.ValueObject.AccountId
import Infrastructure.Factories.BankAccount.EventStorePendingAccountFactory
import Test.Hspec

spec :: Spec
spec = do
  describe "createPendingAccount" $ do
    it "should create a PendingAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let reason = Just "Pending verification"

      -- WHEN
      result <- createPendingAccount uuid currentTime reason

      -- THEN
      case result of
        Right pendingAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkPendingAccount accountId currentTime reason
          pendingAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid PendingAccount, but got error: " ++ show err

    it "should handle missing reason" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let reason = Nothing

      -- WHEN
      result <- createPendingAccount uuid currentTime reason

      -- THEN
      case result of
        Right pendingAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkPendingAccount accountId currentTime reason
          pendingAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid PendingAccount, but got error: " ++ show err
