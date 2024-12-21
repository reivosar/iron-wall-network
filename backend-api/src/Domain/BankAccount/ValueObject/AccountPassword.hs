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
import Domain.ValueError (ValueError, mkValueError)
import Utils.HashGenerator (generateHMAC)

newtype AccountPassword = AccountPassword {unwrapPasswordHash :: Text}
  deriving (Show, Eq)

mkAccountPassword :: Text -> Text -> Either ValueError AccountPassword
mkAccountPassword plainPassword secretKey
  | T.null plainPassword = Left $ mkValueError "Password cannot be empty."
  | otherwise =
      let hashed = generateHMAC plainPassword secretKey
       in Right $ AccountPassword hashed

mkHashedAccountPassword :: Text -> Either ValueError AccountPassword
mkHashedAccountPassword hashedPassword
  | T.null hashedPassword = Left $ mkValueError "Hashed password cannot be empty."
  | otherwise = Right $ AccountPassword hashedPassword

verifyPassword :: AccountPassword -> AccountPassword -> Bool
verifyPassword (AccountPassword inputHash) (AccountPassword storedHash) =
  inputHash == storedHash
