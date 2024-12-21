{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.BalanceSpec (spec) where

import Domain.BankAccount.ValueObject.Balance
import Domain.ValueError (mkValueError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkBalance" $ do
    it "should create a Balance for 0 (threshold)" $ do
      mkBalance 0.0 `shouldBe` mkBalance 0.0

    it "should create a Balance for 1 (threshold)" $ do
      mkBalance 1.0 `shouldBe` mkBalance 1.0

    it "should return an error for -1 (threshold)" $ do
      mkBalance (-1.0) `shouldBe` Left (mkValueError "Balance cannot be negative.")

  describe "addBalance" $ do
    it "should add the amount correctly when the added value is 0 (threshold)" $ do
      let Right initialBalance = mkBalance 100.0
      addBalance initialBalance 0.0 `shouldBe` mkBalance 100.0

    it "should add the amount correctly when the added value is positive (1)" $ do
      let Right initialBalance = mkBalance 100.0
      addBalance initialBalance 1.0 `shouldBe` mkBalance 101.0

    it "should return an error for adding -1 (threshold)" $ do
      let Right initialBalance = mkBalance 100.0
      addBalance initialBalance (-1.0) `shouldBe` Left (mkValueError "Cannot add a negative amount to balance.")

  describe "subtractBalance" $ do
    it "should subtract correctly when the subtracted value is 0 (threshold)" $ do
      let Right initialBalance = mkBalance 100.0
      subtractBalance initialBalance 0.0 `shouldBe` mkBalance 100.0

    it "should subtract correctly when the subtracted value is less than the balance (1)" $ do
      let Right initialBalance = mkBalance 100.0
      subtractBalance initialBalance 1.0 `shouldBe` mkBalance 99.0

    it "should return an error when the subtracted value is negative (-1)" $ do
      let Right initialBalance = mkBalance 100.0
      subtractBalance initialBalance (-1.0) `shouldBe` Left (mkValueError "Cannot subtract a negative amount from balance.")

    it "should return an error when the subtracted value equals the balance (insufficient balance test)" $ do
      let Right initialBalance = mkBalance 100.0
      subtractBalance initialBalance 100.0 `shouldBe` mkBalance 0.0

    it "should return an error when the subtracted value exceeds the balance by 1 (threshold)" $ do
      let Right initialBalance = mkBalance 100.0
      subtractBalance initialBalance 101.0 `shouldBe` Left (mkValueError "Insufficient balance for this transaction.")
