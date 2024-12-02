module Application.Auth.Services.TokenService where

import Application.Auth.Services.AuthUserDto (AuthUserDto)
import Application.Auth.Services.UserAccessTokenDto (UserAccessTokenDto)
import Application.Auth.Services.UserRefreshTokenDto (UserRefreshTokenDto)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

class TokenService m where
  findUserByUsername :: Text -> m (Maybe AuthUserDto)
  createAccessToken :: Int -> Text -> m ()
  createRefreshToken :: Int -> Text -> m ()
  findAccessTokenByUserId :: Int -> m (Maybe UserAccessTokenDto)
  findRefreshTokenByRefreshToken :: Text -> m (Maybe UserRefreshTokenDto)
  removeTokensByUserId :: Int -> m ()
  invalidateTokensByUserId :: Int -> m ()
