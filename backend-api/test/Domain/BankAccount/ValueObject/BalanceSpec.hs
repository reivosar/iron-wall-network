{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.BalanceSpec (spec) where

import Domain.BankAccount.ValueObject.Balance
import Domain.ValueError (mkValueError, unwrapValueError)
import Test.Hspec

spec :: Spec
spec = do
  describe "mkBalance" $ do
    it "should create a Balance for 0 (threshold)" $ do
      case mkBalance 0.0 of
        Right balance -> unwrapBalance balance `shouldBe` 0.0
        Left _ -> expectationFailure "Expected a valid Balance for 0"

    it "should create a Balance for 1 (threshold)" $ do
      case mkBalance 1.0 of
        Right balance -> unwrapBalance balance `shouldBe` 1.0
        Left _ -> expectationFailure "Expected a valid Balance for 1"

    it "should return an error for -1 (threshold)" $ do
      case mkBalance (-1.0) of
        Left err -> unwrapValueError err `shouldBe` "Balance cannot be negative."
        Right _ -> expectationFailure "Expected a validation error for -1"

  describe "addBalance" $ do
    it "should add the amount correctly when the added value is 0 (threshold)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> addBalance balance 0.0 `shouldBe` mkBalance 100.0
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

    it "should add the amount correctly when the added value is positive (1)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> addBalance balance 1.0 `shouldBe` mkBalance 101.0
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

    it "should return an error for adding -1 (threshold)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> addBalance balance (-1.0) `shouldBe` Left (mkValueError "Cannot add a negative amount to balance.")
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

  describe "subtractBalance" $ do
    it "should subtract correctly when the subtracted value is 0 (threshold)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> subtractBalance balance 0.0 `shouldBe` mkBalance 100.0
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

    it "should subtract correctly when the subtracted value is less than the balance (1)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> subtractBalance balance 1.0 `shouldBe` mkBalance 99.0
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

    it "should return an error when the subtracted value is negative (-1)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> subtractBalance balance (-1.0) `shouldBe` Left (mkValueError "Cannot subtract a negative amount from balance.")
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

    it "should subtract the entire balance when the subtracted value equals the balance" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> subtractBalance balance 100.0 `shouldBe` mkBalance 0.0
        Left _ -> expectationFailure "Expected a valid Balance for initial value"

    it "should return an error when the subtracted value exceeds the balance by 1 (threshold)" $ do
      let initialBalance = mkBalance 100.0
      case initialBalance of
        Right balance -> subtractBalance balance 101.0 `shouldBe` Left (mkValueError "Insufficient balance for this transaction.")
        Left _ -> expectationFailure "Expected a valid Balance for initial value"
