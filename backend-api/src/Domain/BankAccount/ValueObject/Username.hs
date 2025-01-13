{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.Username
  ( Username,
    mkUsername,
    unwrapUsername,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.Error (DomainError, mkDomainError)

newtype Username = Username {unwrapUsername :: Text}
  deriving (Show, Eq)

mkUsername :: Text -> Either DomainError Username
mkUsername input = Username <$> validateUsername input

validateUsername :: Text -> Either DomainError Text
validateUsername input =
  let trimmedInput = T.strip input
      actualLen = T.length trimmedInput
   in if T.null trimmedInput
        then Left $ mkDomainError "Username cannot be empty or whitespace."
        else
          if actualLen > 100
            then Left $ mkDomainError $ "Username cannot exceed 100 characters (got " <> pack (show actualLen) <> ")."
            else Right trimmedInput
