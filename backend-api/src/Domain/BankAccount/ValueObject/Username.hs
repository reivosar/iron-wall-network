module Domain.BankAccount.ValueObject.Username
  ( Username,
    mkUsername,
    unwrapUsername,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError, mkValueError)

newtype Username = Username {unwrapUsername :: Text}
  deriving (Show, Eq)

mkUsername :: Text -> Either ValueError Username
mkUsername input = Username <$> validateUsername input

validateUsername :: Text -> Either ValueError Text
validateUsername input =
  let trimmedInput = T.strip input
      actualLen = T.length trimmedInput
   in if T.null trimmedInput
        then Left $ mkValueError "Username cannot be empty or whitespace."
        else
          if actualLen > 100
            then Left $ mkValueError $ "Username cannot exceed 100 characters (got " ++ show actualLen ++ ")."
            else Right trimmedInput
