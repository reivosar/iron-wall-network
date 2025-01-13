{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.Email
  ( Email,
    mkEmail,
    unwrapEmail,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError, mkDomainError)
import Text.Regex.TDFA ((=~))

newtype Email = Email {unwrapEmail :: Text}
  deriving (Show, Eq)

mkEmail :: Text -> Either DomainError Email
mkEmail input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkDomainError "Email cannot be empty or whitespace."
        else
          if not (T.unpack trimmedInput =~ emailRegex)
            then Left $ mkDomainError "Invalid email format."
            else Right $ Email trimmedInput

emailRegex :: String
emailRegex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
