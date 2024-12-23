module Application.Auth.Services.AuthService (AuthService (..), AuthServiceError (..)) where

import Application.Auth.Services.AuthUserDto (AuthUserDto)
import Application.Auth.Services.CreateAccessTokenResult (CreateAccessTokenResult)
import Application.Auth.Services.RecreateAccessTokenResult (RecreateAccessTokenResult)
import Application.Auth.Services.UserAccessTokenDto (UserAccessTokenDto)
import Application.Auth.Services.UserRefreshTokenDto (UserRefreshTokenDto)
import Data.Either ()
import Data.String ()
import Data.Text (Text)

class AuthService m where
  findUserByUsername :: Text -> Result m (Maybe AuthUserDto)
  createAccessToken :: Int -> Result m CreateAccessTokenResult
  recreateAccessToken :: Int -> Result m RecreateAccessTokenResult
  findAccessTokenByAccessToken :: Text -> Result m (Maybe UserAccessTokenDto)
  findRefreshTokenByRefreshToken :: Text -> Result m (Maybe UserRefreshTokenDto)
  validateToken :: Text -> Result m ()
  invalidateToken :: Text -> Result m ()

type Result m a = m (Either AuthServiceError a)

data AuthServiceError
  = UserNotFound Text
  | DatabaseError Text
  | TokenGenerationError Text
  | TokenNotFoundError Text
  | TokenExpiredError Text
  deriving (Show, Eq)
