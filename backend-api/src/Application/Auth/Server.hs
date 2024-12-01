module Application.Auth.Server where

import Application.Api (AuthApi)
import Application.ApiError (convertApiErrorToHttpError)
import Application.Auth.CommandHandler
import Application.Auth.Commands.Login as Login
import Application.Auth.Commands.RefreshToken as RefreshToken
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant

authServer :: Server AuthApi
authServer =
  loginHandler
    :<|> refreshTokenHandler
    :<|> logoutHandler
    :<|> verifyTokenHandler

loginHandler :: Login.LoginRequest -> Handler Login.TokenResponse
loginHandler cmd = do
  result <- liftIO $ handleLogin cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right tokenResponse -> return tokenResponse

refreshTokenHandler :: RefreshToken.RefreshTokenRequest -> Handler RefreshToken.RefreshTokenResponse
refreshTokenHandler cmd = do
  result <- liftIO $ handleRefreshToken cmd
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right tokenResponse -> return tokenResponse

logoutHandler :: Maybe Text -> Handler NoContent
logoutHandler (Just token) = do
  result <- liftIO $ handleLogout token
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent
logoutHandler Nothing = throwError err401

verifyTokenHandler :: Maybe Text -> Handler NoContent
verifyTokenHandler (Just token) = do
  result <- liftIO $ handleVerifyToken token
  case result of
    Left apiError -> throwError $ convertApiErrorToHttpError apiError
    Right _ -> return NoContent
verifyTokenHandler Nothing = throwError err401
