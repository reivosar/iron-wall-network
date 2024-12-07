{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.AccountPassword
  ( AccountPassword,
    mkAccountPassword,
    mkHashedAccountPassword,
    verifyPassword,
    unwrapPasswordHash,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError (..))
import Utils.Env (getEnvTextOrThrow)
import Utils.HashGenerator (generateHMAC)

newtype AccountPassword = AccountPassword {unwrapPasswordHash :: Text}
  deriving (Show, Eq)

mkAccountPassword :: Text -> IO (Either ValueError AccountPassword)
mkAccountPassword plainPassword
  | T.null plainPassword = pure $ Left $ ValueError "Password cannot be empty."
  | otherwise = do
      secretKey <- getEnvTextOrThrow "PASSWORD_SECRET_KEY"
      let hashed = generateHMAC plainPassword secretKey
      pure $ Right $ AccountPassword hashed

mkHashedAccountPassword :: Text -> Either ValueError AccountPassword
mkHashedAccountPassword hashedPassword
  | T.null hashedPassword = Left $ ValueError "Hashed password cannot be empty."
  | otherwise = Right $ AccountPassword hashedPassword

verifyPassword :: Text -> AccountPassword -> IO (Either ValueError Bool)
verifyPassword plainPassword (AccountPassword storedHash)
  | T.null plainPassword = pure $ Left $ ValueError "Password cannot be empty."
  | otherwise = do
      secretKey <- getEnvTextOrThrow "PASSWORD_SECRET_KEY"
      let hashed = generateHMAC plainPassword secretKey
      pure $ Right (hashed == storedHash)
