{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.PostalCode
  ( PostalCode,
    mkPostalCode,
    unwrapPostalCode,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError, mkDomainError)
import Text.Regex.TDFA ((=~))

newtype PostalCode = PostalCode {unwrapPostalCode :: Text}
  deriving (Show, Eq)

mkPostalCode :: Text -> Either DomainError PostalCode
mkPostalCode input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkDomainError "Postal code cannot be empty or whitespace."
        else
          if not (T.unpack trimmedInput =~ postalCodeRegex)
            then Left $ mkDomainError "Invalid postal code format. Expected format: 123-4567."
            else Right $ PostalCode trimmedInput

postalCodeRegex :: String
postalCodeRegex = "^[0-9]{3}-[0-9]{4}$"
