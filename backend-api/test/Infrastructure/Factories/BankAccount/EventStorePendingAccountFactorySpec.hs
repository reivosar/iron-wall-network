{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStorePendingAccountFactorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.PendingAccount
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.Error (DomainError, mkDomainError, unwrapDomainError)
import Infrastructure.Factories.BankAccount.EventStorePendingAccountFactory
import Test.Hspec

-- Mock environment for testing
data MockEnv = MockEnv
  { mockTryPend :: AccountId -> IO (Either DomainError ())
  }

instance BankAccountService (ReaderT MockEnv IO) where
  tryCreate _ = return $ Left $ mkDomainError "tryCreate not implemented in MockEnv"
  tryApprove _ = return $ Left $ mkDomainError "tryApprove not implemented in MockEnv"
  tryActivate _ = return $ Left $ mkDomainError "tryActivate not implemented in MockEnv"
  tryPend accntId = do
    env <- ask
    liftIO $ mockTryPend env accntId
  trySuspend _ = return $ Left $ mkDomainError "trySuspend not implemented in MockEnv"
  tryClose _ = return $ Left $ mkDomainError "tryClose not implemented in MockEnv"

spec :: Spec
spec = do
  describe "createPendingAccount" $ do
    it "should create a PendingAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let reason = Just "Pending verification"
      let mockEnv =
            MockEnv
              { mockTryPend = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createPendingAccount uuid currentTime reason) mockEnv

      -- THEN
      case result of
        Right pendingAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkPendingAccount accountId currentTime reason
          pendingAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid PendingAccount, but got error: " ++ show err

    it "should handle missing reason" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let reason = Nothing
      let mockEnv =
            MockEnv
              { mockTryPend = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createPendingAccount uuid currentTime reason) mockEnv

      -- THEN
      case result of
        Right pendingAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkPendingAccount accountId currentTime reason
          pendingAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid PendingAccount, but got error: " ++ show err

    it "should return an error if tryPend fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let reason = Just "Invalid reason"
      let mockEnv =
            MockEnv
              { mockTryPend = \_ -> return $ Left $ mkDomainError "Pendency not allowed"
              }

      -- WHEN
      result <- runReaderT (createPendingAccount uuid currentTime reason) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Pendency not allowed"
        Right _ -> expectationFailure "Expected an error, but got a valid PendingAccount"
