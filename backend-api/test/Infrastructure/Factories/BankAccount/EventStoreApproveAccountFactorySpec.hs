{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreApproveAccountFactorySpec (spec) where

import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.ApproveAccount
import Domain.BankAccount.ValueObject.AccountId
import Infrastructure.Factories.BankAccount.EventStoreApproveAccountFactory
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
      result <- createApproveAccount uuid currentTime givenlNotes

      -- THEN
      let accntId = mkAccountId uuid
      let expectedAccount = mkApproveAccount accntId currentTime givenlNotes
      result `shouldBe` Right expectedAccount

    it "should handle missing approval notes" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let givenlNotes = Nothing

      -- WHEN
      result <- createApproveAccount uuid currentTime givenlNotes

      -- THEN
      let accntId = mkAccountId uuid
      let expectedAccount = mkApproveAccount accntId currentTime givenlNotes
      result `shouldBe` Right expectedAccount
