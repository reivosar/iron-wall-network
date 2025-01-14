{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Factories.BankAccount.EventStoreCloseAccountFactorySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import Domain.BankAccount.Entity.CloseAccount
import Domain.BankAccount.Services.BankAccountService
import Domain.BankAccount.ValueObject.AccountId
import Domain.Error (DomainError, mkDomainError, unwrapDomainError)
import Infrastructure.Factories.BankAccount.EventStoreCloseAccountFactory
import Test.Hspec

-- Mock environment for testing
data MockEnv = MockEnv
  { mockTryClose :: AccountId -> IO (Either DomainError ())
  }

instance BankAccountService (ReaderT MockEnv IO) where
  tryCreate _ = return $ Left $ mkDomainError "tryCreate not implemented in MockEnv"
  tryApprove _ = return $ Left $ mkDomainError "tryApprove not implemented in MockEnv"
  tryActivate _ = return $ Left $ mkDomainError "tryActivate not implemented in MockEnv"
  tryPend _ = return $ Left $ mkDomainError "tryPend not implemented in MockEnv"
  trySuspend _ = return $ Left $ mkDomainError "trySuspend not implemented in MockEnv"
  tryClose accountId = do
    env <- ask
    liftIO $ mockTryClose env accountId

spec :: Spec
spec = do
  describe "createCloseAccount" $ do
    it "should create a CloseAccount with valid inputs" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let closureReason = Just "User requested closure"
      let mockEnv =
            MockEnv
              { mockTryClose = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createCloseAccount uuid currentTime closureReason) mockEnv

      -- THEN
      case result of
        Right closeAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkCloseAccount accountId currentTime closureReason
          closeAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid CloseAccount, but got error: " ++ show err

    it "should handle missing closure reason" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let closureReason = Nothing
      let mockEnv =
            MockEnv
              { mockTryClose = \_ -> return $ Right ()
              }

      -- WHEN
      result <- runReaderT (createCloseAccount uuid currentTime closureReason) mockEnv

      -- THEN
      case result of
        Right closeAccount -> do
          let accountId = mkAccountId uuid
          let expectedAccount = mkCloseAccount accountId currentTime closureReason
          closeAccount `shouldBe` expectedAccount
        Left err -> expectationFailure $ "Expected a valid CloseAccount, but got error: " ++ show err

    it "should return an error if tryClose fails" $ do
      -- GIVEN
      let uuid = UUID.nil
      currentTime <- getCurrentTime
      let closureReason = Just "Invalid closure request"
      let mockEnv =
            MockEnv
              { mockTryClose = \_ -> return $ Left $ mkDomainError "Closure not allowed"
              }

      -- WHEN
      result <- runReaderT (createCloseAccount uuid currentTime closureReason) mockEnv

      -- THEN
      case result of
        Left err -> unwrapDomainError err `shouldBe` "Closure not allowed"
        Right _ -> expectationFailure "Expected an error, but got a valid CloseAccount"
