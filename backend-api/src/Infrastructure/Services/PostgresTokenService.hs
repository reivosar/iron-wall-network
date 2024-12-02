module Infrastructure.Services.PostgresTokenService where

import qualified Application.Auth.Services.AuthUserDto as AuthUserDto
import Application.Auth.Services.TokenService
import qualified Application.Auth.Services.UserAccessTokenDto as UserAccessTokenDto
import qualified Application.Auth.Services.UserRefreshTokenDto as UserRefreshTokenDto
import Data.String (fromString)
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple
import Infrastructure.Database.DbUtils (connectDb)

instance TokenService IO where
  findUserByUsername username = return Nothing

  createAccessToken _ _ = return ()

  createRefreshToken _ _ = return ()

  findAccessTokenByUserId _ = return Nothing

  findRefreshTokenByRefreshToken _ = return Nothing

  removeTokensByUserId _ = return ()

  invalidateTokensByUserId _ = return ()
