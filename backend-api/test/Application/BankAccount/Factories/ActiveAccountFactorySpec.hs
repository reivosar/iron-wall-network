{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.BankAccount.Factories.ActiveAccountFactorySpec (spec) where

import Application.BankAccount.Factories.ActiveAccountFactory (createActiveAccount)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import Domain.BankAccount.Entity.ActiveAccount
import Domain.BankAccount.ValueObject.AccountId
import Domain.BankAccount.ValueObject.AccountPassword
import Domain.ValueError (unwrapValueError)
import Test.Hspec
import Utils.Env

spec :: Spec
spec = do
  describe "createActiveAccount" $ do
    it "should create an ActiveAccount with valid inputs" $ do
      -- GIVEN
      uuid <- liftIO UUID.nextRandom
      currentTime <- liftIO getCurrentTime
      let password = "valid-password"
      setEnvText "PASSWORD_SECRET_KEY" "test-secret-key"

      -- WHEN
      result <- createActiveAccount uuid password currentTime

      -- THEN
      case result of
        Right activeAccount -> do
          let accountId = mkAccountId uuid
          let accountPassword = either (error . ("Failed to create AccountPassword: " ++) . show) id (mkAccountPassword password "test-secret-key")
          let expectedAccount = mkActiveAccount accountId accountPassword currentTime
          activeAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid ActiveAccount, but got error: " ++ show err

    it "should return an error if the password is empty" $ do
      -- GIVEN
      uuid <- liftIO UUID.nextRandom
      currentTime <- liftIO getCurrentTime
      let password = ""
      setEnvText "PASSWORD_SECRET_KEY" "test-secret-key"

      -- WHEN
      result <- createActiveAccount uuid password currentTime

      -- THEN
      case result of
        Left err -> unwrapValueError err `shouldBe` "Password cannot be empty."
        Right _ -> expectationFailure "Expected an error, but got a valid ActiveAccount"

    it "should return an error if PASSWORD_SECRET_KEY is not set" $ do
      -- GIVEN
      uuid <- liftIO UUID.nextRandom
      currentTime <- liftIO getCurrentTime
      let password = "valid-password"
      setEnvText "PASSWORD_SECRET_KEY" ""

      -- WHEN & THEN
      createActiveAccount uuid password currentTime
        `shouldThrow` ( \case
                          EnvNotFound msg -> msg == "Environment variable not found: PASSWORD_SECRET_KEY"
                      )
