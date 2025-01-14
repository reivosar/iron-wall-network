{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreSuspendAccountFactorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.SuspendAccount
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.Error (DomainError, mkDomainError, unwrapDomainError)
import Infrastructure.Factories.BankAccount.EventStoreSuspendAccountFactory
import Test.Hspec

-- Mock environment for testing
data MockEnv = MockEnv
  { mockTrySuspend :: AccountId -> IO (Either DomainError ())
  }

instance BankAccountService (ReaderT MockEnv IO) where
  tryCreate _ = return $ Left $ mkDomainError "tryCreate not implemented in MockEnv"
  tryApprove _ = return $ Left $ mkDomainError "tryApprove not implemented in MockEnv"
  tryActivate _ = return $ Left $ mkDomainError "tryActivate not implemented in MockEnv"
  tryPend _ = return $ Left $ mkDomainError "tryPend not implemented in MockEnv"
  trySuspend accntId = do
    env <- ask
    liftIO $ mockTrySuspend env accntId
  tryClose _ = return $ Left $ mkDomainError "tryClose not implemented in MockEnv"

spec :: Spec
spec = do
  describe "createSuspendAccount" $ do
    it "should create a SuspendAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let suspensionReason = Just "Suspicious activity detected"
      let mockEnv =
            MockEnv
              { mockTrySuspend = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createSuspendAccount uuid currentTime suspensionReason) mockEnv

      -- THEN
      case result of
        Right suspendAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkSuspendAccount accountId currentTime suspensionReason
          suspendAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid SuspendAccount, but got error: " ++ show err

    it "should handle missing suspension reason" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let suspensionReason = Nothing
      let mockEnv =
            MockEnv
              { mockTrySuspend = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createSuspendAccount uuid currentTime suspensionReason) mockEnv

      -- THEN
      case result of
        Right suspendAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkSuspendAccount accountId currentTime suspensionReason
          suspendAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid SuspendAccount, but got error: " ++ show err

    it "should return an error if trySuspend fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let suspensionReason = Just "Invalid reason"
      let mockEnv =
            MockEnv
              { mockTrySuspend = \_ -> return $ Left $ mkDomainError "Suspension not allowed"
              }

      -- WHEN
      result <- runReaderT (createSuspendAccount uuid currentTime suspensionReason) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Suspension not allowed"
        Right _ -> expectationFailure "Expected an error, but got a valid SuspendAccount"
