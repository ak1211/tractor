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
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
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
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Proxy                               ( Proxy(..) )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import qualified Database.Persist.MySQL        as MySQL
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Logger                       ( ApacheLogger
                                                          , withStdoutLogger
                                                          )
import           Servant                                  ( (:>)
                                                          , (:<|>)(..)
                                                          , Context(..)
                                                          , Delete
                                                          , Get
                                                          , Header'
                                                          , JSON
                                                          , NoContent
                                                          , Patch
                                                          , PostAccepted
                                                          , Put
                                                          , Raw
                                                          , ReqBody
                                                          , Required
                                                          , Strict
                                                          , Summary
                                                          )
import qualified Servant
import qualified Servant.Auth.Server
import           Servant.CSV.Cassava                      ( CSV )
import           Servant.HTML.Lucid                       ( HTML )

import qualified Conf
import           NetService.ApiTypes                      ( ApiOhlcv
                                                          , ApiPortfolio
                                                          , AuthClientId(..)
                                                          , AuthenticatedUser(..)
                                                          , CaptureMarketCode
                                                          , ChartWithCacheControl
                                                          , QueryHeight
                                                          , QueryLimit
                                                          , QueryTimeFrame
                                                          , QueryWidth
                                                          , RespAuth(..)
                                                          , SVG
                                                          , SystemHealth(..)
                                                          , SystemSignal(..)
                                                          )
import qualified NetService.ApiTypes           as ApiTypes
import           NetService.HomePage                      ( HomePage(..) )
import           NetService.HttpError
import           NetService.RestApiHandler
import qualified NetService.UserAuth
import           NetService.WebServerConfig

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
        :> "api" :> "v1" :> "token" :> HAuthorization :> Get '[JSON] RespAuth
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
type Protected
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
 :<|> "api" :> "v1" :> "auth" :> "clientid" :> Get '[JSON] AuthClientId
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] ApiTypes.VerRev
 :<|> "api" :> "v1" :> "health" :> Get '[JSON] SystemHealth
 :<|> "api" :> "v1" :> "portfolios" :> Get '[JSON] [ApiPortfolio]
 :<|> "api" :> "v1" :> "stocks" :> "chart" :> CaptureMarketCode :> QueryTimeFrame :> QueryWidth :> QueryHeight :> Get '[SVG] ChartWithCacheControl

-- |
--
type TokenEndpoint
    ="api" :> "v1" :> "token" :> Get '[JSON] RespAuth

-- |
--
protected
    :: Config
    -> Servant.Auth.Server.AuthResult AuthenticatedUser
    -> Servant.Server Protected
protected cnf result = case result of
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
        :<|> Servant.serveDirectoryFileServer "frontend-elm/public"
        :<|> return (AuthClientId clientId)
        :<|> return (cVerRev cnf)
        :<|> return (SystemHealth Green)
        :<|> getPortfolioHandler (cPool cnf)
        :<|> getChartHandler (cPool cnf)
    where clientId = Conf.clientID . Conf.slack . cConf $ cnf

-- |
--
tokenEndpoint
    :: Config
    -> Servant.Auth.Server.AuthResult AuthenticatedUser
    -> Servant.Server TokenEndpoint
tokenEndpoint cnf result = case result of
    Servant.Auth.Server.Authenticated auser -> do
        jwt <- jwtAccessToken auser
        M.liftIO $ print jwt
        pure $ RespAuth
            { accessToken = T.pack $ BL8.unpack jwt
            , expiresIn   = expIn
            , tokenType   = "Bearer"
            }
    --
    -- Catch-all for `BadPassword`, `NoSuchUser`, and `Indefinite`
    --
    _ -> Servant.Auth.Server.throwAll
        (Servant.Auth.Server.wwwAuthenticatedErr "Access to auth endpoint")
  where
    --
    --
    jwtAccessToken
        :: ApiTypes.AuthenticatedUser -> Servant.Handler BL8.ByteString
    jwtAccessToken oauthrep = do
        expiration <- Time.addUTCTime (fromIntegral expIn)
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
    expIn :: Int
    expIn = 24 * 60 * 60

-- |
--
type WebApi
    = (Servant.Auth.Server.Auth '[Servant.Auth.Server.BasicAuth] AuthenticatedUser :> TokenEndpoint)
 :<|> (Servant.Auth.Server.Auth '[Servant.Auth.Server.JWT] AuthenticatedUser :> Protected)
 :<|> Unprotected

-- |
--
webApiServer :: Config -> Servant.Server WebApi
webApiServer cfg = tokenEndpoint cfg :<|> protected cfg :<|> unprotected cfg

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
        config  = Config versionRevision jwtCfg conf pool chan
        authCfg = NetService.UserAuth.authCheck config
        context =
            jwtCfg
                :. Servant.Auth.Server.defaultCookieSettings
                :. authCfg
                :. Servant.EmptyContext
        apiServer     = webApiServer config
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



