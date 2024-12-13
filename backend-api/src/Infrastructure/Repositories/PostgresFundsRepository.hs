{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repositories.PostgresFundsRepository
  ( findById,
    save,
  )
where

import Control.Exception ()
import Control.Monad.IO.Class (liftIO)
import Data.Scientific ()
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID ()
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
  ( FromField (..),
    ResultError (..),
    returnError,
  )
import Database.PostgreSQL.Simple.FromRow
  ( FromRow (..),
    field,
  )
import qualified Domain.BankAccount.Entity.Funds as Funds
import Domain.BankAccount.Repositories.FundsRepository (FundsRepository (..))
import Domain.BankAccount.ValueObject.AccountId
  ( AccountId,
    mkAccountId,
    unwrapAccountId,
  )
import qualified Domain.BankAccount.ValueObject.Balance as Balance
import Domain.ValueError (ValueError (..))
import Infrastructure.Database.Executor
import Text.Read (readMaybe)

instance FromField AccountId where
  fromField f mdata = do
    rawUuid <- fromField f mdata
    case mkAccountId rawUuid of
      Right accountId -> pure accountId
      Left _ -> returnError ConversionFailed f "Invalid AccountId format"

instance FromField Balance.Balance where
  fromField f mdata = case mdata of
    Nothing -> returnError UnexpectedNull f ""
    Just bs -> case readMaybe (unpack (decodeUtf8 bs)) of
      Just dbl -> case Balance.mkBalance dbl of
        Right balance -> pure balance
        Left _ -> returnError ConversionFailed f "Failed to create Balance"
      Nothing -> returnError ConversionFailed f "Failed to parse numeric value to Double"

instance FromRow Funds.Funds where
  fromRow =
    Funds.Funds
      <$> field
      <*> field

instance FundsRepository IO where
  findById accountId = do
    let rawAccountId = unwrapAccountId accountId

    result <-
      liftIO $
        fetchOne
          "SELECT account_id, balance FROM bank_balance WHERE account_id = ?"
          [rawAccountId]

    case result of
      Left err -> pure $ Left err
      Right Nothing -> pure $ Right Nothing
      Right (Just funds) -> pure $ Right (Just funds)

  save _ = pure $ Right ()
