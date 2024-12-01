{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.ApiError
  ( ApiError (..),
    convertToApiError,
    convertValueErrorToApiError,
    convertSystemErrorToApiError,
    convertUseCaseErrorToApiError,
    convertApiErrorToHttpError,
  )
where

import qualified Application.UseCaseError as UseCaseError
import Data.Aeson (ToJSON, encode)
import Data.Text (Text, pack)
import Domain.ValueError (ValueError (..), formatError)
import GHC.Generics (Generic)
import Servant.Server (ServerError (..), err400, err401, err404, err500)

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

convertApiErrorToHttpError :: ApiError -> ServerError
convertApiErrorToHttpError (ClientError code message) = case code of
  400 -> err400 {errBody = encode message}
  401 -> err401 {errBody = encode message}
  404 -> err404 {errBody = encode message}
  _ -> err400 {errBody = encode message} -- Default to 400 for unknown client errors
convertApiErrorToHttpError (SystemError code message) = case code of
  500 -> err500 {errBody = encode message}
  _ -> err500 {errBody = encode message} -- Default to 500 for unknown system errors
