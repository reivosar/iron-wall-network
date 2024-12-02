{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application.Auth.Api where

import Application.ApiError (convertApiErrorToHttpError)
import Application.Auth.Commands.LoginCommand as LoginCommand
import Application.Auth.Commands.RefreshTokenCommand as RefreshTokenCommand
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

type AuthApi =
  "auth" :> "login" :> ReqBody '[JSON] LoginCommand.LoginRequest :> Post '[JSON] LoginCommand.TokenResponse
    :<|> "auth" :> "refresh" :> ReqBody '[JSON] RefreshTokenCommand.RefreshTokenRequest :> Post '[JSON] RefreshTokenCommand.RefreshTokenResponse
    :<|> "auth" :> "logout" :> Header "Authorization" Text :> Post '[JSON] NoContent
    :<|> "auth" :> "verify" :> Header "Authorization" Text :> Get '[JSON] NoContent
