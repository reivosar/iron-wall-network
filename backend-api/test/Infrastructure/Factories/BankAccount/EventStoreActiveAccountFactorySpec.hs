{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreActiveAccountFactorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import Domain.BankAccount.Entity.ActiveAccount
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.BankAccount.ValueObject.AccountPassword
import Domain.Error (DomainError, mkDomainError, unwrapDomainError)
import Infrastructure.Factories.BankAccount.EventStoreActiveAccountFactory
import Test.Hspec
import Utils.Env

-- Mock environment for testing
data MockEnv = MockEnv
  { mockTryActivate :: AccountId -> IO (Either DomainError ())
  }

instance BankAccountService (ReaderT MockEnv IO) where
  tryCreate _ = return $ Left $ mkDomainError "tryCreate not implemented in MockEnv"
  tryApprove _ = return $ Left $ mkDomainError "tryApprove not implemented in MockEnv"
  tryActivate accntId = do
    env <- ask
    liftIO $ mockTryActivate env accntId
  tryPend _ = return $ Left $ mkDomainError "tryPend not implemented in MockEnv"
  trySuspend _ = return $ Left $ mkDomainError "trySuspend not implemented in MockEnv"
  tryClose _ = return $ Left $ mkDomainError "tryClose not implemented in MockEnv"

spec :: Spec
spec = do
  describe "createActiveAccount" $ do
    it "should create an ActiveAccount with valid inputs" $ do
      -- GIVEN
      uuid <- liftIO UUID.nextRandom
      currentTime <- liftIO getCurrentTime
      let password = "valid-password"
      setEnvText "PASSWORD_SECRET_KEY" "test-secret-key"
      let mockEnv =
            MockEnv
              { mockTryActivate = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createActiveAccount uuid password currentTime) mockEnv

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
      let mockEnv =
            MockEnv
              { mockTryActivate = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createActiveAccount uuid password currentTime) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Password cannot be empty."
        Right _ -> expectationFailure "Expected an error, but got a valid ActiveAccount"

    it "should return an error if PASSWORD_SECRET_KEY is not set" $ do
      -- GIVEN
      uuid <- liftIO UUID.nextRandom
      currentTime <- liftIO getCurrentTime
      let password = "valid-password"
      setEnvText "PASSWORD_SECRET_KEY" ""
      let mockEnv =
            MockEnv
              { mockTryActivate = \_ -> return $ Right ()
              }

      -- WHEN & THEN
      runReaderT (createActiveAccount uuid password currentTime) mockEnv
        `shouldThrow` ( \case
                          EnvNotFound msg -> msg == "Environment variable not found: PASSWORD_SECRET_KEY"
                      )
