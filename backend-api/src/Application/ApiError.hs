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

import qualified Application.TokenInvalidationError as TokenInvalidationError
import qualified Application.UseCaseError as UseCaseError
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Domain.ValueError (ValueError (..), formatError)
import GHC.Generics (Generic)
import Servant.Server (ServerError (..), err400, err401, err404, err500)

data ApiError
  = ClientError {code :: Int, message :: Text}
  | SystemError {code :: Int, message :: Text}
  | TokenInvalidationError {code :: Int, message :: Text}
  deriving (Show, Generic, ToJSON)

clientErrorCode :: Int
clientErrorCode = 400

systemErrorCode :: Int
systemErrorCode = 500

convertToApiError :: (Show e) => Int -> e -> ApiError
convertToApiError statusCode err
  | statusCode >= systemErrorCode =
      SystemError
        { code = systemErrorCode,
          message = pack (show err)
        }
  | otherwise =
      ClientError
        { code = clientErrorCode,
          message = pack (show err)
        }

convertValueErrorToApiError :: ValueError -> ApiError
convertValueErrorToApiError valueError =
  ClientError
    { code = clientErrorCode,
      message = formatError valueError
    }

convertSystemErrorToApiError :: (Show e) => e -> ApiError
convertSystemErrorToApiError err =
  SystemError
    { code = systemErrorCode,
      message = pack (show err)
    }

convertUseCaseErrorToApiError :: UseCaseError.UseCaseError -> ApiError
convertUseCaseErrorToApiError (UseCaseError.ValidationError msg) =
  ClientError
    { code = clientErrorCode,
      message = msg
    }
convertUseCaseErrorToApiError (UseCaseError.SystemError msg) =
  SystemError
    { code = systemErrorCode,
      message = msg
    }

convertApiErrorToHttpError :: ApiError -> ServerError
convertApiErrorToHttpError (ClientError code message) = case code of
  400 -> err400 {errBody = BL.fromStrict $ encodeUtf8 message}
  401 -> err401 {errBody = BL.fromStrict $ encodeUtf8 message}
  404 -> err404 {errBody = BL.fromStrict $ encodeUtf8 message}
  _ -> err400 {errBody = BL.fromStrict $ encodeUtf8 message}
convertApiErrorToHttpError (SystemError code message) = case code of
  500 -> err500 {errBody = BL.fromStrict $ encodeUtf8 message}
  _ -> err500 {errBody = BL.fromStrict $ encodeUtf8 message}
convertApiErrorToHttpError (TokenInvalidationError _ message) = err400 {errBody = BL.fromStrict $ encodeUtf8 message}
