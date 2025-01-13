{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.City
  ( City,
    mkCity,
    unwrapCity,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.Error (DomainError, mkDomainError)

newtype City = City {unwrapCity :: Text}
  deriving (Show, Eq)

mkCity :: Text -> Either DomainError City
mkCity input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkDomainError "City cannot be empty or whitespace."
        else
          if T.length trimmedInput > 255
            then Left $ mkDomainError $ "City cannot exceed 255 characters (got " <> pack (show (T.length trimmedInput)) <> ")."
            else Right $ City trimmedInput
