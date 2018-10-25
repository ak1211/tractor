{-
    This file is part of Tractor.

    Tractor is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Tractor is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with Tractor.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
Module      :  NetService.UserAuth
Description :  REST Web API user authentication
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

user authentication モジュールです
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.UserAuth
    ( authCheck
    )
where
import           Control.Exception.Safe
import qualified Control.Monad.IO.Class        as M
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Monoid                              ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TLB
import qualified Network.HTTP.Conduit          as N
import qualified Network.HTTP.Types.Header     as Header
import qualified Network.URI                   as URI
import           Servant.Auth.Server                      ( AuthResult(..)
                                                          , BasicAuthCfg
                                                          , BasicAuthData(..)
                                                          , FromBasicAuthData(..)
                                                          )

import qualified BrokerBackend                 as BB
import qualified Conf
import           NetService.ApiTypes                      ( AuthTempCode(..)
                                                          , AuthenticatedUser(..)
                                                          , OAuthAccessResponse(..)
                                                          , UsersInfoResponse(..)
                                                          )
import qualified NetService.ApiTypes           as ApiTypes
import           NetService.WebServerConfig


type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)
instance FromBasicAuthData AuthenticatedUser where
    fromBasicAuthData authData authCheckFunction = authCheckFunction authData

-- |
-- Basic認証で提出された user : password形式の clientid : temporary_code をSlackに問い合わせる
--
authCheck :: Config -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheck cnf (BasicAuthData clientid temporaryCode) =
    go
        `catch` (\(StringException msg _) -> do
                    print msg
                    pure Indefinite
                )
  where
    go           = if idMatches then exchangeCode else pure NoSuchUser
    --
    --
    idMatches    = B8.unpack clientid == T.unpack myClientId
    --
    --
    myClientId   = Conf.clientID . Conf.slack $ cConf cnf
    --
    --
    exchangeCode = do
        -- Slack API (oauth.access) に問い合わせる
        oauthResp <- sendRequestToOAuthAccess
            cnf
            (AuthTempCode $ B8.unpack temporaryCode)
        -- Slack access token & user id
        let uid  = respUserId (oauthResp :: OAuthAccessResponse)
            pair = (,) <$> respAccessToken oauthResp <*> uid
        -- Slack API (users.info) に問い合わせる
        uInfoResp <- maybe (throwString "fail to access on Slack API")
                           (uncurry sendRequestToUsersInfo)
                           pair
        let myToken   = Conf.oauthAccessToken . Conf.slack $ cConf cnf
            testToken = respAccessToken oauthResp
        case packAuthenticatedUser oauthResp uInfoResp of
            -- 自分のSlackトークンと一致を確認できれば認証
            Just auser | testToken == Just myToken -> pure (Authenticated auser)
                       | otherwise                 -> pure BadPassword
            Nothing -> pure Indefinite

-- |
--
packAuthenticatedUser
    :: OAuthAccessResponse
    -> UsersInfoResponse
    -> Maybe ApiTypes.AuthenticatedUser
packAuthenticatedUser x y = do
    userId       <- respUserId (x :: OAuthAccessResponse)
    userName     <- respUserName (y :: UsersInfoResponse)
    userRealName <- respUserRealName y
    userTz       <- respUserTz y
    userTzOffset <- respUserTzOffset y
    Just ApiTypes.AuthenticatedUser {..}

-- |
-- send request to https://slack.com/api/oauth.access
-- https://api.slack.com/docs/sign-in-with-slack#obtain_an_access_token
--
sendRequestToOAuthAccess
    :: (M.MonadIO m, MonadThrow m)
    => Config
    -> AuthTempCode
    -> m OAuthAccessResponse
sendRequestToOAuthAccess cnf (AuthTempCode tempCode) = do
    uri       <- maybe (throwString "URI parse fail.") pure methodURI
    json      <- Aeson.decode <$> M.liftIO (sendApiRequest uri)
    oauthResp <- maybe (throwString "API \"oauth.access\" fail.") pure json
    if respOk (oauthResp :: OAuthAccessResponse)
        then return oauthResp
        else throwString (BL8.unpack $ errMsg oauthResp)
  where
    --
    --
    errMsg :: OAuthAccessResponse -> BL8.ByteString
    errMsg x = maybe "oauth.access error" (BL8.pack . T.unpack)
        $ respError (x :: OAuthAccessResponse)
    --
    --
    methodURI =
        URI.parseURI
            .  TL.unpack
            .  TLB.toLazyText
            $  "https://"
            <> "slack.com/api/oauth.access"
            <> "?"
            <> "client_id="
            <> cID
            <> "&"
            <> "client_secret="
            <> cSecret
            <> "&"
            <> "code="
            <> TLB.fromString tempCode
    cID     = TLB.fromText . Conf.clientID . Conf.slack $ cConf cnf
    cSecret = TLB.fromText . Conf.clientSecret . Conf.slack $ cConf cnf

-- |
-- send request to https://slack.com/api/users.info
-- https://api.slack.com/methods/users.info
--
sendRequestToUsersInfo
    :: (M.MonadIO m, MonadThrow m) => T.Text -> T.Text -> m UsersInfoResponse
sendRequestToUsersInfo accessToken userId = do
    uri       <- maybe (throwString "URI parse fail.") pure methodURI
    json      <- Aeson.decode <$> M.liftIO (sendApiRequest uri)
    uInfoResp <- maybe (throwString "API \"users.info\" fail.") pure json
    if respOk (uInfoResp :: UsersInfoResponse)
        then return uInfoResp
        else throwString $ errMsg uInfoResp
  where
    --
    --
    errMsg :: UsersInfoResponse -> String
    errMsg x =
        maybe "users.info error" T.unpack $ respError (x :: UsersInfoResponse)
    --
    --
    methodURI =
        URI.parseURI
            .  TL.unpack
            .  TLB.toLazyText
            $  "https://"
            <> "slack.com/api/users.info"
            <> "?"
            <> "token="
            <> TLB.fromText accessToken
            <> "&"
            <> "user="
            <> TLB.fromText userId

--
--
sendApiRequest :: URI.URI -> IO BL8.ByteString
sendApiRequest uri = do
    -- HTTPS接続ですよ
    manager   <- N.newManager N.tlsManagerSettings
    -- Slack APIへアクセスする
    httpsResp <- BB.fetchHTTP manager [thisContentType] Nothing [] uri
    -- レスポンスボディにはJSON応答が入っている
    return $ N.responseBody httpsResp
  where
    thisContentType =
        (Header.hContentType, "application/x-www-form-urlencoded")

