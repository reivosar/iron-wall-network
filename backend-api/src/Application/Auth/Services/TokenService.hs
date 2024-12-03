module Application.Auth.Services.TokenService where

import Application.Auth.Services.AuthUserDto (AuthUserDto)
import Application.Auth.Services.CreateAccessTokenResult (CreateAccessTokenResult)
import Application.Auth.Services.RecreateAccessTokenResult (RecreateAccessTokenResult)
import Application.Auth.Services.UserAccessTokenDto (UserAccessTokenDto)
import Application.Auth.Services.UserRefreshTokenDto (UserRefreshTokenDto)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (Either)
import Data.String (String)
import Data.Text (Text)
import GHC.Generics (Generic)

class TokenService m where
  findUserByUsername :: Text -> Result m (Maybe AuthUserDto)
  createAccessToken :: Int -> Result m CreateAccessTokenResult
  recreateAccessToken :: Int -> Result m RecreateAccessTokenResult
  findAccessTokenByUserId :: Int -> Result m (Maybe UserAccessTokenDto)
  findRefreshTokenByRefreshToken :: Text -> Result m (Maybe UserRefreshTokenDto)
  removeTokensByUserId :: Int -> Result m ()
  invalidateTokensByUserId :: Int -> Result m ()

type Result m a = m (Either TokenServiceError a)

data TokenServiceError
  = UserNotFound String
  | DatabaseError String
  | TokenGenerationError String
  deriving (Show, Eq)
