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
Module      :  NetService.WebServer
Description :  REST Web API server
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

Web API サーバーモジュールです
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
module NetService.WebServer
    ( runWebServer
    , ApiForFrontend
    , ApiForDocument
    )
where
import qualified Control.Monad.IO.Class        as M
import qualified Control.Monad.Logger          as Logger
import           Control.Monad.Trans.Resource             ( runResourceT )
import qualified Crypto.JOSE.JWA.JWS           as Jose
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Monoid                              ( (<>) )
import           Data.Proxy                               ( Proxy(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as TLB
import qualified Data.Time                     as Time
import qualified Database.Persist.MySQL        as MySQL
import qualified Network.HTTP.Conduit          as N
import qualified Network.HTTP.Types.Header     as Header
import qualified Network.URI                   as URI
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Logger                       ( ApacheLogger
                                                          , withStdoutLogger
                                                          )
import           Servant                                  ( (:>)
                                                          , (:<|>)(..)
                                                          , Context(..)
                                                          , JSON
                                                          , Get
                                                          , Delete
                                                          , Post
                                                          , Raw
                                                          , ReqBody
                                                          , NoContent
                                                          , Put
                                                          , Summary
                                                          , PostAccepted
                                                          , Header'
                                                          , Strict
                                                          , Required
                                                          , Patch
                                                          )
import qualified Servant
import qualified Servant.Auth.Server
import           Servant.CSV.Cassava                      ( CSV )
import           Servant.HTML.Lucid                       ( HTML )

import qualified BrokerBackend                 as BB
import qualified Conf
import           NetService.ApiTypes                      ( RespAuth(..)
                                                          , CaptureMarketCode
                                                          , QueryTimeFrame
                                                          , ApiOhlcv
                                                          , ApiPortfolio
                                                          , AuthTempCode(..)
                                                          , AuthClientId(..)
                                                          , AuthenticatedUser(..)
                                                          , OAuthAccessResponse(..)
                                                          ,ChartWithCacheControl
                                                          , QueryLimit
                                                          , QueryWidth
                                                          , QueryHeight
                                                          , SVG
                                                          , SystemSignal(..)
                                                          , SystemHealth(..)
                                                          , UsersInfoResponse(..)
                                                          )
import qualified NetService.ApiTypes           as ApiTypes
import           NetService.HttpError
import           NetService.RestApiHandler
import           NetService.HomePage                      ( HomePage(..) )

-- |
-- このサーバーで使う設定情報
data Config = Config
    { cVerRev :: ApiTypes.VerRev
    , cJWTS   :: Servant.Auth.Server.JWTSettings
    , cConf   :: Conf.Info
    , cPool   :: MySQL.ConnectionPool
    , cChan   :: ApiTypes.ServerTChan
    }

-- |
--
type HAuthorization = Header' '[Required, Strict] "Authorization" T.Text

type ApiForFrontend
    = Summary "publish price on the zeroMQ infrastructure."
        :> "api" :> "v1" :> "publish" :> "zmq" :> CaptureMarketCode :> HAuthorization :> Put '[JSON] NoContent
 :<|> Summary "This endpoint runs the crawler."
        :> "api" :> "v1" :> "stocks" :> "history" :> "all" :> PostAccepted '[JSON] NoContent
 :<|> Summary "Select prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> QueryLimit :> HAuthorization :> Get '[JSON, CSV] [ApiOhlcv]
 :<|> Summary "Insert prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> HAuthorization :> ReqBody '[JSON, CSV] [ApiOhlcv] :> Put '[JSON] [ApiOhlcv]
 :<|> Summary "Update / Insert prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> HAuthorization :> ReqBody '[JSON] [ApiOhlcv] :> Patch '[JSON] [ApiOhlcv]
 :<|> Summary "Delete prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> HAuthorization :> ReqBody '[JSON] [ApiOhlcv] :> Delete '[JSON] [ApiOhlcv]
 :<|> Summary "get access token (JWT)"
        :> "api" :> "v1" :> "auth" :> ReqBody '[JSON] AuthTempCode :> Post '[JSON] RespAuth
 :<|> Summary "get OAuth client id"
        :> "api" :> "v1" :> "auth" :> "clientid" :> Get '[JSON] AuthClientId
 :<|> Summary "get system version"
        :> "api" :> "v1" :> "version" :> Get '[JSON] ApiTypes.VerRev
 :<|> Summary "get system health"
        :> "api" :> "v1" :> "health" :> Get '[JSON] SystemHealth
 :<|> Summary "get portfolios"
        :> "api" :> "v1" :> "portfolios" :> Get '[JSON] [ApiPortfolio]

type ApiForDocument
    = ApiForFrontend
 :<|> Summary "Get Chart, suffix is only .svg"
       :> "api" :> "v1" :> "stocks" :> "chart" :> CaptureMarketCode :> QueryTimeFrame :> QueryWidth :> QueryHeight :> Get '[SVG] ChartWithCacheControl

-- |
--
type ProtectedWithJWT
    = "api" :> "v1" :> "publish" :> "zmq" :> CaptureMarketCode :> Put '[JSON] NoContent
 :<|> "api" :> "v1" :> "stocks" :> "history" :> "all" :> PostAccepted '[JSON] NoContent
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> QueryLimit :> Get '[JSON, CSV] [ApiOhlcv]
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> ReqBody '[JSON, CSV] [ApiOhlcv] :> Put '[JSON] [ApiOhlcv]
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> ReqBody '[JSON] [ApiOhlcv] :> Patch '[JSON] [ApiOhlcv]
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CaptureMarketCode :> QueryTimeFrame :> ReqBody '[JSON] [ApiOhlcv] :> Delete '[JSON] [ApiOhlcv]

-- |
--
type Unprotected
    = Get '[HTML] HomePage
 :<|> "public" :> Raw
 :<|> "api" :> "v1" :> "auth" :> ReqBody '[JSON] AuthTempCode :> Post '[JSON] RespAuth
 :<|> "api" :> "v1" :> "auth" :> "clientid" :> Get '[JSON] AuthClientId
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] ApiTypes.VerRev
 :<|> "api" :> "v1" :> "health" :> Get '[JSON] SystemHealth
 :<|> "api" :> "v1" :> "portfolios" :> Get '[JSON] [ApiPortfolio]
 :<|> "api" :> "v1" :> "stocks" :> "chart" :> CaptureMarketCode :> QueryTimeFrame :> QueryWidth :> QueryHeight :> Get '[SVG] ChartWithCacheControl

-- |
--
protectedWithJWT
    :: Config
    -> Servant.Auth.Server.AuthResult AuthenticatedUser
    -> Servant.Server ProtectedWithJWT
protectedWithJWT cnf result = case result of
    Servant.Auth.Server.Authenticated _ ->
        publishZmqHandler (cChan cnf) (cPool cnf)
            :<|> updateHistoriesHandler (cConf cnf)
            :<|> getHistoriesHandler (cPool cnf)
            :<|> putHistoriesHandler (cPool cnf)
            :<|> patchHistoriesHandler (cPool cnf)
            :<|> deleteHistoriesHandler (cPool cnf)
    Servant.Auth.Server.BadPassword ->
        Servant.Auth.Server.throwAll (wwwAuthenticatedErr "bad password")
    Servant.Auth.Server.NoSuchUser ->
        Servant.Auth.Server.throwAll (wwwAuthenticatedErr "no such user")
    Servant.Auth.Server.Indefinite ->
        Servant.Auth.Server.throwAll (wwwAuthenticatedErr "indefinite")
  where
    wwwAuthenticatedErr desc = Servant.err401
        { Servant.errHeaders = [("WWW-Authenticate", description desc)]
        }
    description x =
        let desc = B8.concat ["error_description=\"", x, "\""]
        in  B8.intercalate
                ", "
                [ "Bearer realm=\"Protected API\""
                , "error=\"invalid_token\""
                , desc
                ]

-- |
--
unprotected :: Config -> Servant.Server Unprotected
unprotected cnf =
    return HomePage
        :<|> Servant.serveDirectoryFileServer "frontend/public"
        :<|> postAuthTempCodeHandler cnf
        :<|> return (AuthClientId clientId)
        :<|> return (cVerRev cnf)
        :<|> return (SystemHealth Green)
        :<|> getPortfolioHandler (cPool cnf)
        :<|> getChartHandler (cPool cnf)
    where clientId = Conf.clientID . Conf.slack . cConf $ cnf

-- |
--
type WebApi
    = (Servant.Auth.Server.Auth '[Servant.Auth.Server.JWT] AuthenticatedUser :> ProtectedWithJWT)
 :<|> Unprotected

-- |
--
webApiServer :: Config -> Servant.Server WebApi
webApiServer cfg = protectedWithJWT cfg :<|> unprotected cfg

-- |
-- Web API サーバーを起動する
runWebServer :: ApiTypes.VerRev -> Conf.Info -> ApiTypes.ServerTChan -> IO ()
runWebServer versionRevision conf chan = do
    pool  <- Logger.runNoLoggingT . runResourceT $ createPool
    myKey <- Servant.Auth.Server.generateKey
    let api    = Proxy :: Proxy WebApi
        jwtCfg = (Servant.Auth.Server.defaultJWTSettings myKey)
            { Servant.Auth.Server.jwtAlg = Just Jose.HS512
            }
        context =
            jwtCfg
                :. Servant.Auth.Server.defaultCookieSettings
                :. Servant.EmptyContext
        apiServer = webApiServer (Config versionRevision jwtCfg conf pool chan)
        servantServer = Servant.serveWithContext api context apiServer
    withStdoutLogger
        $ \aplogger -> Warp.runSettings (settings aplogger) servantServer
  where
    --
    --
    createPool = MySQL.createMySQLPool connInfo poolSize
    connInfo   = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize   = 8
    --
    --
    settings :: ApacheLogger -> Warp.Settings
    settings aplogger =
        Warp.setPort 8739 $ Warp.setLogger aplogger Warp.defaultSettings


-- |
-- 仮コードをアクセストークンに交換する
postAuthTempCodeHandler :: Config -> AuthTempCode -> Servant.Handler RespAuth
postAuthTempCodeHandler cnf tempCode = do
    oauthResp <- sendRequestToOAuthAccess cnf tempCode
    let uid  = respUserId (oauthResp :: OAuthAccessResponse)
        pair = (,) <$> respAccessToken oauthResp <*> uid
    uInfoResp <- maybe err401Unauthorized (uncurry sendRequestToUsersInfo) pair
    M.liftIO $ print uInfoResp
    --
    let myToken   = Conf.oauthAccessToken . Conf.slack $ cConf cnf
        testToken = respAccessToken oauthResp
    case authenticatedUser oauthResp uInfoResp of
        Just aUser
            | testToken == Just myToken -> do
                -- 自分のSlackトークンと一致
                jwt <- makeJWT aUser
                M.liftIO $ print jwt
                pure $ RespAuth
                    { accessToken = T.pack $ BL8.unpack jwt
                    , expiresIn   = expiresIn
                    , tokenType   = "Bearer"
                    }
            | otherwise -> err403Forbidden
        Nothing -> err401Unauthorized
  where
    --
    --
    authenticatedUser
        :: OAuthAccessResponse
        -> UsersInfoResponse
        -> Maybe ApiTypes.AuthenticatedUser
    authenticatedUser x y = do
        userId       <- respUserId (x :: OAuthAccessResponse)
        userName     <- respUserName (y :: UsersInfoResponse)
        userRealName <- respUserRealName y
        userTz       <- respUserTz y
        userTzOffset <- respUserTzOffset y
        Just ApiTypes.AuthenticatedUser {..}
    --
    --
    makeJWT :: ApiTypes.AuthenticatedUser -> Servant.Handler BL8.ByteString
    makeJWT oauthrep = do
        expiration <- Time.addUTCTime (fromIntegral expiresIn)
            <$> M.liftIO Time.getCurrentTime
        jwt <- M.liftIO
            (Servant.Auth.Server.makeJWT oauthrep settings $ Just expiration)
        either err ok jwt
      where
        settings = cJWTS cnf
        err _ = err401Unauthorized
        ok = pure
    --
    --
    expiresIn :: Int
    expiresIn = 2 * 60 * 60

-- |
-- send request to https://slack.com/api/oauth.access
-- https://api.slack.com/docs/sign-in-with-slack#obtain_an_access_token
--
sendRequestToOAuthAccess
    :: Config -> AuthTempCode -> Servant.Handler OAuthAccessResponse
sendRequestToOAuthAccess cnf (AuthTempCode tempCode) = do
    uri       <- maybe err500InternalServerError pure methodURI
    json      <- Aeson.decode <$> M.liftIO (sendApiRequest uri)
    oauthResp <- maybe
        (err400BadRequest "\"oauth.access\" json parse error.")
        pure
        json
    if respOk (oauthResp :: OAuthAccessResponse)
        then do
            M.liftIO $ print oauthResp
            return oauthResp
        else err400BadRequest (errMsg oauthResp)
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
sendRequestToUsersInfo :: T.Text -> T.Text -> Servant.Handler UsersInfoResponse
sendRequestToUsersInfo accessToken userId = do
    uri <- maybe err500InternalServerError pure methodURI
    M.liftIO $ print uri
    json      <- Aeson.decode <$> M.liftIO (sendApiRequest uri)
    uInfoResp <- maybe (err400BadRequest "\"users.info\" json parse error")
                       pure
                       json
    if respOk (uInfoResp :: UsersInfoResponse)
        then do
            M.liftIO $ print uInfoResp
            return uInfoResp
        else err400BadRequest (errMsg uInfoResp)
  where
    --
    --
    errMsg :: UsersInfoResponse -> BL8.ByteString
    errMsg x = maybe "users.info error" (BL8.pack . T.unpack)
        $ respError (x :: UsersInfoResponse)
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

