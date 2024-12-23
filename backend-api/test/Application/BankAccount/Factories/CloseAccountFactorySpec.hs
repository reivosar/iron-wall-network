{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.Factories.CloseAccountFactorySpec (spec) where

import Application.BankAccount.Factories.CloseAccountFactory
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.CloseAccount
import Domain.BankAccount.ValueObject.AccountId
import Test.Hspec

spec :: Spec
spec = do
  describe "createCloseAccount" $ do
    it "should create a CloseAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let closureReason = Just "User requested closure"

      -- WHEN
      let result = createCloseAccount uuid currentTime closureReason

      -- THEN
      case result of
        Right closeAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkCloseAccount accountId currentTime closureReason
          closeAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid CloseAccount, but got error: " ++ show err

    it "should handle missing closure reason" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let closureReason = Nothing

      -- WHEN
      let result = createCloseAccount uuid currentTime closureReason

      -- THEN
      case result of
        Right closeAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkCloseAccount accountId currentTime closureReason
          closeAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid CloseAccount, but got error: " ++ show err
