module Application.Auth.CommandHandler where

import Application.ApiError (ApiError, convertTokenInvalidationErrorToApiError, convertUseCaseErrorToApiError)
import qualified Application.Auth.Commands.LoginCommand as LoginCommand
import qualified Application.Auth.Commands.RefreshTokenCommand as RefreshTokenCommand
import Application.Auth.TokenValidator (validateToken)
import qualified Application.Auth.UseCases.LoginUseCase as LoginUseCase
import qualified Application.Auth.UseCases.LogoutUseCase as LogoutUseCase
import qualified Application.Auth.UseCases.RefreshTokenUseCase as RefreshTokenUseCase
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Infrastructure.Services.PostgresAuthService

-- Login Handler
handleLogin :: LoginCommand.LoginRequest -> IO (Either ApiError LoginCommand.TokenResponse)
handleLogin cmd = do
  currentTime <- getCurrentTime
  let input =
        LoginUseCase.Input
          { LoginUseCase.userName = LoginCommand.userName cmd,
            LoginUseCase.password = LoginCommand.password cmd,
            LoginUseCase.authKey = LoginCommand.authKey cmd
          }
  result <- LoginUseCase.execute input
  return $ first convertUseCaseErrorToApiError (fmap convertLoginUseCaseOutputToTokenResponse result)

convertLoginUseCaseOutputToTokenResponse :: LoginUseCase.Output -> LoginCommand.TokenResponse
convertLoginUseCaseOutputToTokenResponse (LoginUseCase.Output accessToken refreshToken accessTokenExpiresAt refreshTokenExpiresAt) =
  LoginCommand.TokenResponse accessToken refreshToken accessTokenExpiresAt refreshTokenExpiresAt

-- Refresh Token Handler
handleRefreshToken :: RefreshTokenCommand.RefreshTokenRequest -> IO (Either ApiError RefreshTokenCommand.RefreshTokenResponse)
handleRefreshToken cmd = do
  let input =
        RefreshTokenUseCase.Input
          { RefreshTokenUseCase.refreshToken = RefreshTokenCommand.refreshToken cmd
          }
  result <- RefreshTokenUseCase.execute input
  return $ first convertUseCaseErrorToApiError (fmap convertLoginUseCaseOutputToRefreshTokenResponse result)

convertLoginUseCaseOutputToRefreshTokenResponse :: RefreshTokenUseCase.Output -> RefreshTokenCommand.RefreshTokenResponse
convertLoginUseCaseOutputToRefreshTokenResponse (RefreshTokenUseCase.Output accessToken expiresAt) =
  RefreshTokenCommand.RefreshTokenResponse accessToken expiresAt

-- Logout Handler
handleLogout :: Text -> IO (Either ApiError ())
handleLogout token = do
  let input = LogoutUseCase.Input {LogoutUseCase.token = token}
  result <- LogoutUseCase.execute input
  return $ first convertUseCaseErrorToApiError result

-- Verify Token Handler
handleVerifyToken :: Text -> IO (Either ApiError ())
handleVerifyToken token = do
  return $ Right ()
