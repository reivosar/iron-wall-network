{-# LANGUAGE OverloadedStrings #-}

module Utils.EnvSpec (spec) where

import Control.Exception (try)
import Test.Hspec
import Utils.Env

spec :: Spec
spec = do
  describe "getEnvText" $ do
    it "should return the value of an existing environment variable as Text" $ do
      setEnvText "TEST_ENV_VAR" "test-value"
      result <- getEnvText "TEST_ENV_VAR"
      case result of
        Right actual -> actual `shouldBe` "test-value"
        Left _ -> expectationFailure "Expected a valid environment variable value"

    it "should return an error if the environment variable does not exist" $ do
      result <- getEnvText "NON_EXISTENT_ENV_VAR"
      case result of
        Left (EnvNotFound msg) -> msg `shouldBe` "Environment variable not found: NON_EXISTENT_ENV_VAR"
        Right _ -> expectationFailure "Expected an error for missing environment variable"

  describe "getEnvTextWithDefault" $ do
    it "should return the value of an existing environment variable" $ do
      setEnvText "TEST_ENV_VAR" "test-value"
      result <- getEnvTextWithDefault "TEST_ENV_VAR" "default-value"
      result `shouldBe` "test-value"

    it "should return the default value if the environment variable does not exist" $ do
      result <- getEnvTextWithDefault "NON_EXISTENT_ENV_VAR" "default-value"
      result `shouldBe` "default-value"

  describe "getEnvTextOrThrow" $ do
    it "should return the value of an existing environment variable" $ do
      setEnvText "TEST_ENV_VAR" "test-value"
      result <- getEnvTextOrThrow "TEST_ENV_VAR"
      result `shouldBe` "test-value"

    it "should throw an exception if the environment variable does not exist" $ do
      action <- try (getEnvTextOrThrow "NON_EXISTENT_ENV_VAR")
      case action of
        Left (EnvNotFound msg) -> msg `shouldBe` "Environment variable not found: NON_EXISTENT_ENV_VAR"
        Right _ -> expectationFailure "Expected an exception for missing environment variable"

  describe "getEnvString" $ do
    it "should return the value of an existing environment variable as String" $ do
      setEnvText "TEST_ENV_VAR" "test-value"
      result <- getEnvString "TEST_ENV_VAR"
      case result of
        Right actual -> actual `shouldBe` "test-value"
        Left _ -> expectationFailure "Expected a valid environment variable value"

    it "should return an error if the environment variable does not exist" $ do
      result <- getEnvString "NON_EXISTENT_ENV_VAR"
      case result of
        Left (EnvNotFound msg) -> msg `shouldBe` "Environment variable not found: NON_EXISTENT_ENV_VAR"
        Right _ -> expectationFailure "Expected an error for missing environment variable"

  describe "getEnvStringWithDefault" $ do
    it "should return the value of an existing environment variable" $ do
      setEnvText "TEST_ENV_VAR" "test-value"
      result <- getEnvStringWithDefault "TEST_ENV_VAR" "default-value"
      result `shouldBe` "test-value"

    it "should return the default value if the environment variable does not exist" $ do
      result <- getEnvStringWithDefault "NON_EXISTENT_ENV_VAR" "default-value"
      result `shouldBe` "default-value"

  describe "getEnvStringOrThrow" $ do
    it "should return the value of an existing environment variable" $ do
      setEnvText "TEST_ENV_VAR" "test-value"
      result <- getEnvStringOrThrow "TEST_ENV_VAR"
      result `shouldBe` "test-value"

    it "should throw an exception if the environment variable does not exist" $ do
      action <- try (getEnvStringOrThrow "NON_EXISTENT_ENV_VAR")
      case action of
        Left (EnvNotFound msg) -> msg `shouldBe` "Environment variable not found: NON_EXISTENT_ENV_VAR"
        Right _ -> expectationFailure "Expected an exception for missing environment variable"

  describe "setEnvText" $ do
    it "should set the value of an environment variable" $ do
      setEnvText "TEST_ENV_VAR" "new-value"
      result <- getEnvText "TEST_ENV_VAR"
      case result of
        Right actual -> actual `shouldBe` "new-value"
        Left _ -> expectationFailure "Expected the environment variable to be set correctly"
