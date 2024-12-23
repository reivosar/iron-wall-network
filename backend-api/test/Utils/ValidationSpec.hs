{-# LANGUAGE OverloadedStrings #-}

module Utils.ValidationSpec (spec) where

import Test.Hspec
import Utils.Validation

spec :: Spec
spec = do
  describe "validateNotEmpty" $ do
    it "should pass for a non-empty input" $ do
      validateNotEmpty "hello" `shouldBe` True

    it "should fail for an empty input" $ do
      validateNotEmpty "" `shouldBe` False

    it "should fail for an input with only whitespace" $ do
      validateNotEmpty "    " `shouldBe` False

  describe "validateRegex" $ do
    it "should pass for input matching the pattern" $ do
      validateRegex "abc123" "^[a-zA-Z0-9]+$" `shouldBe` True

    it "should fail for input not matching the pattern" $ do
      validateRegex "abc-123" "^[a-zA-Z0-9]+$" `shouldBe` False

  describe "validateMaxLength" $ do
    it "should pass for input within the max length" $ do
      validateMaxLength 5 "hello" `shouldBe` True

    it "should fail for input exceeding the max length" $ do
      validateMaxLength 5 "hello world" `shouldBe` False

  describe "validateMinLength" $ do
    it "should pass for input meeting the minimum length" $ do
      validateMinLength 5 "hello" `shouldBe` True

    it "should fail for input shorter than the minimum length" $ do
      validateMinLength 5 "hi" `shouldBe` False

  describe "validateLengthRange" $ do
    it "should pass for input within the length range" $ do
      validateLengthRange 3 5 "hey" `shouldBe` True

    it "should fail for input shorter than the minimum length" $ do
      validateLengthRange 3 5 "hi" `shouldBe` False

    it "should fail for input longer than the maximum length" $ do
      validateLengthRange 3 5 "hello world" `shouldBe` False

  describe "validateAllowedCharacters" $ do
    it "should pass for input with allowed characters only" $ do
      validateAllowedCharacters "abc123" "abcdefghijklmnopqrstuvwxyz0123456789" `shouldBe` True

    it "should fail for input with disallowed characters" $ do
      validateAllowedCharacters "abc-123" "abcdefghijklmnopqrstuvwxyz0123456789" `shouldBe` False
