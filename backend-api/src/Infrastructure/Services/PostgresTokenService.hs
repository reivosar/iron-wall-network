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
  findUserByUsername _ = return $ Right Nothing

  createAccessToken _ = return $ Left (TokenGenerationError "Not implemented")

  recreateAccessToken _ = return $ Left (TokenGenerationError "Not implemented")

  findAccessTokenByUserId _ = return $ Right Nothing

  findRefreshTokenByRefreshToken _ = return $ Right Nothing

  removeTokensByUserId _ = return $ Right ()

  invalidateTokensByUserId _ = return $ Right ()
