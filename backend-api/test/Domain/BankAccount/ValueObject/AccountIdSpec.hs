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
      let actual = mkAccountId uuid
      let expected = mkAccountId uuid
      actual `shouldBe` expected
