{-# LANGUAGE MultiParamTypeClasses #-}

module Domain.BankAccount.Services.UsernameUniquenessValidator
  ( UsernameUniquenessValidator (..),
  )
where

import Data.Text (Text)
import Domain.BankAccount.ValueObject.Username (Username)
import Domain.Error (DomainError)

class (Monad m) => UsernameUniquenessValidator m where
  validateUsernameUniqueness :: Username -> m (Either DomainError ())
