module Domain.BankAccount.ValueObject.FullName
  ( FullName,
    mkFullName,
    unwrapFullName,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.ValueError (ValueError (..))

newtype FullName = FullName {unwrapFullName :: Text}
  deriving (Show, Eq)

mkFullName :: Text -> Either ValueError FullName
mkFullName input =
  let trimmedInput = T.strip input
      actualLen = T.length trimmedInput
   in if T.null trimmedInput
        then Left $ ValueError "Full name cannot be empty or whitespace."
        else
          if actualLen > 255
            then Left $ ValueError $ "Full name cannot exceed 255 characters (got " ++ show actualLen ++ ")."
            else Right $ FullName trimmedInput
