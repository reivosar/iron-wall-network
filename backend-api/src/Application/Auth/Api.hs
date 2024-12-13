{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application.Auth.Api (AuthApi) where

import Application.Auth.Commands.LoginCommand as LoginCommand
import Application.Auth.Commands.RefreshTokenCommand as RefreshTokenCommand
import Data.Text (Text)
import Servant

type AuthApi =
  "v1" :> "auth" :> "login" :> ReqBody '[JSON] LoginCommand.LoginRequest :> Post '[JSON] LoginCommand.TokenResponse
    :<|> "v1" :> "auth" :> "refresh" :> ReqBody '[JSON] RefreshTokenCommand.RefreshTokenRequest :> Post '[JSON] RefreshTokenCommand.RefreshTokenResponse
    :<|> "v1" :> "auth" :> "logout" :> Header "Authorization" Text :> Post '[JSON] NoContent
    :<|> "v1" :> "auth" :> "verify" :> Header "Authorization" Text :> Get '[JSON] NoContent
