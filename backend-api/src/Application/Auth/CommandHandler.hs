module Application.Auth.CommandHandler where

import Application.ApiError (ApiError, convertTokenInvalidationErrorToApiError, convertUseCaseErrorToApiError)
import qualified Application.Auth.Commands.Login as Login
import qualified Application.Auth.Commands.RefreshToken as RefreshToken
import Application.Auth.TokenValidator (validateToken)
import qualified Application.Auth.UseCases.LoginUseCase as LoginUseCase
import qualified Application.Auth.UseCases.LogoutUseCase as LogoutUseCase
import qualified Application.Auth.UseCases.RefreshTokenUseCase as RefreshTokenUseCase
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

-- Login Handler
handleLogin :: Login.LoginRequest -> IO (Either ApiError Login.TokenResponse)
handleLogin cmd = do
  currentTime <- getCurrentTime
  let input =
        LoginUseCase.Input
          { LoginUseCase.username = Login.username cmd,
            LoginUseCase.password = Login.password cmd,
            LoginUseCase.authKey = Login.authKey cmd
          }
  result <- LoginUseCase.execute input
  return $ first convertUseCaseErrorToApiError (fmap convertLoginUseCaseOutputToTokenResponse result)

convertLoginUseCaseOutputToTokenResponse :: LoginUseCase.Output -> Login.TokenResponse
convertLoginUseCaseOutputToTokenResponse (LoginUseCase.Output accessToken refreshToken expiresIn) =
  Login.TokenResponse accessToken refreshToken expiresIn

-- Refresh Token Handler
handleRefreshToken :: RefreshToken.RefreshTokenRequest -> IO (Either ApiError RefreshToken.RefreshTokenResponse)
handleRefreshToken cmd = do
  let input =
        RefreshTokenUseCase.Input
          { RefreshTokenUseCase.refreshToken = RefreshToken.refreshToken cmd
          }
  result <- RefreshTokenUseCase.execute input
  return $ first convertUseCaseErrorToApiError (fmap convertLoginUseCaseOutputToRefreshTokenResponse result)

convertLoginUseCaseOutputToRefreshTokenResponse :: RefreshTokenUseCase.Output -> RefreshToken.RefreshTokenResponse
convertLoginUseCaseOutputToRefreshTokenResponse (RefreshTokenUseCase.Output accessToken expiresIn) =
  RefreshToken.RefreshTokenResponse accessToken expiresIn

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
