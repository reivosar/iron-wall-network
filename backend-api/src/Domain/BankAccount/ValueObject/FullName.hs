{-# LANGUAGE OverloadedStrings #-}

module Domain.BankAccount.ValueObject.FullName
  ( FullName,
    mkFullName,
    unwrapFullName,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Domain.ValueError (ValueError, mkValueError)

newtype FullName = FullName {unwrapFullName :: Text}
  deriving (Show, Eq)

mkFullName :: Text -> Either ValueError FullName
mkFullName input = FullName <$> validateFullName input

validateFullName :: Text -> Either ValueError Text
validateFullName input =
  let trimmedInput = T.strip input
      actualLen = T.length trimmedInput
   in if T.null trimmedInput
        then Left $ mkValueError "Full name cannot be empty or whitespace."
        else
          if actualLen > 255
            then Left $ mkValueError $ "Full name cannot exceed 255 characters (got " <> pack (show actualLen) <> ")."
            else Right trimmedInput
