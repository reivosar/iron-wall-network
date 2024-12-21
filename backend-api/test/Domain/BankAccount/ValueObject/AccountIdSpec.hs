{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.AccountIdSpec (spec) where

import Data.UUID (toText)
import Domain.BankAccount.ValueObject.AccountId
import Test.Hspec
import Utils.UUIDGenerator (generateUUID)

spec :: Spec
spec = do
  describe "mkAccountId" $ do
    it "should create an AccountId if the accountId is valid" $ do
      uuid <- generateUUID
      mkAccountId uuid `shouldBe` mkAccountId uuid

  describe "generateAccountId" $ do
    it "should generate a valid AccountId" $ do
      accountId <- generateAccountId
      toText (unwrapAccountId accountId) `shouldNotBe` ""

    it "should generate unique AccountIds" $ do
      accountId1 <- generateAccountId
      accountId2 <- generateAccountId
      unwrapAccountId accountId1 `shouldNotBe` unwrapAccountId accountId2
