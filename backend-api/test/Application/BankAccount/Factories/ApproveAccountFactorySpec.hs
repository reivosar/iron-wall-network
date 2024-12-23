{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.Factories.ApproveAccountFactorySpec (spec) where

import Application.BankAccount.Factories.ApproveAccountFactory
import Data.IORef
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.ApproveAccount
import Domain.BankAccount.ValueObject.AccountId
import Test.Hspec

spec :: Spec
spec = do
  describe "createApproveAccount" $ do
    it "should create an ApproveAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let givenlNotes = Just "Approved by manager"

      -- WHEN
      let result = createApproveAccount uuid currentTime givenlNotes

      -- THEN
      case result of
        Right approveAccount -> do
          let accntId = mkAccountId uuid
          let expectedAccount = mkApproveAccount accntId currentTime givenlNotes
          approveAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid ApproveAccount, but got error: " ++ show err

    it "should handle missing approval notes" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let givenlNotes = Nothing

      -- WHEN
      let result = createApproveAccount uuid currentTime givenlNotes

      -- THEN
      case result of
        Right approveAccount -> do
          let accntId = mkAccountId uuid
          let expectedAccount = mkApproveAccount accntId currentTime givenlNotes
          approveAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid ApproveAccount, but got error: " ++ show err
