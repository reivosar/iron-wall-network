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
import Domain.Error (DomainError, mkDomainError)
import Utils.HashGenerator (generateHMAC)

newtype AccountPassword = AccountPassword {unwrapPasswordHash :: Text}
  deriving (Show, Eq)

mkAccountPassword :: Text -> Text -> Either DomainError AccountPassword
mkAccountPassword plainPassword secretKey
  | T.null plainPassword = Left $ mkDomainError "Password cannot be empty."
  | otherwise =
      let hashed = generateHMAC plainPassword secretKey
       in Right $ AccountPassword hashed

mkHashedAccountPassword :: Text -> Either DomainError AccountPassword
mkHashedAccountPassword hashedPassword
  | T.null hashedPassword = Left $ mkDomainError "Hashed password cannot be empty."
  | otherwise = Right $ AccountPassword hashedPassword

verifyPassword :: AccountPassword -> AccountPassword -> Bool
verifyPassword (AccountPassword inputHash) (AccountPassword storedHash) =
  inputHash == storedHash
