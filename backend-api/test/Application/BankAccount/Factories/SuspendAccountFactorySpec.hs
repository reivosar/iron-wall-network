{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.Factories.SuspendAccountFactorySpec (spec) where

import Application.BankAccount.Factories.SuspendAccountFactory
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.SuspendAccount
import Domain.BankAccount.ValueObject.AccountId
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
      let result = createSuspendAccount uuid currentTime suspensionReason

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
      let result = createSuspendAccount uuid currentTime suspensionReason

      -- THEN
      case result of
        Right suspendAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkSuspendAccount accountId currentTime suspensionReason
          suspendAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid SuspendAccount, but got error: " ++ show err
