{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreSuspendAccountFactorySpec (spec) where

import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.SuspendAccount
import Domain.BankAccount.ValueObject.AccountId
import Infrastructure.Factories.BankAccount.EventStoreSuspendAccountFactory
import Test.Hspec

spec :: Spec
spec = do
  describe "createSuspendAccount" $ do
    it "should create a SuspendAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let suspensionReason = Just "Suspicious activity detected"

      -- WHEN
      result <- createSuspendAccount uuid currentTime suspensionReason

      -- THEN
      case result of
        Right suspendAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkSuspendAccount accountId currentTime suspensionReason
          suspendAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid SuspendAccount, but got error: " ++ show err

    it "should handle missing suspension reason" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let suspensionReason = Nothing

      -- WHEN
      result <- createSuspendAccount uuid currentTime suspensionReason

      -- THEN
      case result of
        Right suspendAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkSuspendAccount accountId currentTime suspensionReason
          suspendAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid SuspendAccount, but got error: " ++ show err
