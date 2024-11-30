{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.ApiError
  ( ApiError (..),
    convertToApiError,
    convertValueErrorToApiError,
    convertSystemErrorToApiError,
    convertUseCaseErrorToApiError,
  )
where

import qualified Application.UseCaseError as UseCaseError
import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import Domain.ValueError (ValueError (..), formatError)
import GHC.Generics (Generic)

data ApiError
  = ClientError {code :: Int, message :: Text}
  | SystemError {code :: Int, message :: Text}
  deriving (Show, Generic, ToJSON)

convertToApiError :: (Show e) => Int -> e -> ApiError
convertToApiError statusCode err
  | statusCode >= 500 =
      SystemError
        { code = statusCode,
          message = pack (show err)
        }
  | otherwise =
      ClientError
        { code = statusCode,
          message = pack (show err)
        }

convertValueErrorToApiError :: ValueError -> ApiError
convertValueErrorToApiError valueError =
  ClientError
    { code = 400,
      message = formatError valueError
    }

convertSystemErrorToApiError :: (Show e) => e -> ApiError
convertSystemErrorToApiError err =
  SystemError
    { code = 500,
      message = pack (show err)
    }

convertUseCaseErrorToApiError :: UseCaseError.UseCaseError -> ApiError
convertUseCaseErrorToApiError (UseCaseError.ValidationError msg) =
  ClientError
    { code = 400,
      message = msg
    }
convertUseCaseErrorToApiError (UseCaseError.SystemError msg) =
  SystemError
    { code = 500,
      message = msg
    }
