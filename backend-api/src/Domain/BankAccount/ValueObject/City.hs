{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.City
  ( City,
    mkCity,
    unwrapCity,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.ValueError (ValueError, mkValueError)

newtype City = City {unwrapCity :: Text}
  deriving (Show, Eq)

mkCity :: Text -> Either ValueError City
mkCity input =
  let trimmedInput = T.strip input
   in if T.null trimmedInput
        then Left $ mkValueError "City cannot be empty or whitespace."
        else
          if T.length trimmedInput > 255
            then Left $ mkValueError $ "City cannot exceed 255 characters (got " <> pack (show (T.length trimmedInput)) <> ")."
            else Right $ City trimmedInput
