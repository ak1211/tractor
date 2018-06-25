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
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.WebServer
    ( runWebServer
    , WebApi
    , proxyWebApiServer
    ) where
import qualified Control.Concurrent.STM       as STM
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as Logger
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy.Char8   as BL8
import           Data.Monoid                  ((<>))
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import           Database.Persist             ((=.), (==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import qualified Network.HTTP.Types.Header    as Header
import qualified Network.URI                  as URI
import qualified Network.URI.Encode           as URI
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Servant
import           Servant.API                  ((:<|>) (..), (:>), Capture,
                                               FormUrlEncoded, Get, Header',
                                               JSON, NoContent, Patch,
                                               PostAccepted, Put, QueryParam',
                                               Raw, ReqBody, Required, Strict)
import           Servant.CSV.Cassava          (CSV)
import qualified Servant.Docs
import qualified Servant.Elm
import           Servant.HTML.Lucid           (HTML)

import qualified BackOffice.Agency            as Agency
import qualified BrokerBackend                as BB
import qualified Conf
import           Model                        (TimeFrame (..))
import qualified Model
import           NetService.ApiTypes          (ApiOhlcv, ApiPortfolio,
                                               OAuthAccessResponse (..))
import qualified NetService.ApiTypes          as ApiTypes
import           NetService.HomePage          (HomePage (..))

-- |
-- このサーバーで使う設定情報
data Config = Config
    { cConf :: Conf.Info
    , cPool :: MySQL.ConnectionPool
    , cChan :: ApiTypes.ServerTChan
    }

-- |
--
type TempCode   = String
type CTempCode  = Capture "tempCode" TempCode
instance Servant.Docs.ToCapture CTempCode where
    toCapture _ =
        Servant.Docs.DocCapture
        "OAuth temporary code"
        "Exchanging a temporary code for an access token"

-- |
--
type MarketCode = String
type CMarketCode= Capture "marketCode" MarketCode
instance Servant.Docs.ToCapture CMarketCode where
    toCapture _ =
        Servant.Docs.DocCapture
        "market code"
        "NI225, TOPIX, TYO8306 etc..."

-- |
--
type QTimeFrame = QueryParam' '[Required, Strict] "tf" TimeFrame

instance Servant.Elm.ElmType TimeFrame

instance Servant.Docs.ToParam QTimeFrame where
    toParam _ =
        Servant.Docs.DocQueryParam
            "tf"
            Model.validTimeFrames
            "prices of a time frame."
            Servant.Docs.Normal

instance Servant.FromHttpApiData TimeFrame where
    parseQueryParam x =
        let y = T.filter ((/=) '"') $ URI.decodeText x
        in
        case Model.toTimeFrame =<< parse y of
            Nothing ->
                Left "TimeFrame"
            Just a ->
                Right a
        where
        --
        --
        parse :: T.Text -> Maybe String
        parse =
            either (const Nothing) Just . Servant.parseQueryParam

instance Servant.ToHttpApiData TimeFrame where
    toQueryParam =
        T.pack . Model.fromTimeFrame

-- |
--
type AuthzValue         = T.Text
type HAuthorization     = Header' '[Required, Strict] "Authorization" AuthzValue

-- |
--
type GetOAuthReply      = Get '[JSON] ApiTypes.OAuthReply
type GetApiPortfolios   = HAuthorization :> Get '[JSON] [ApiPortfolio]
--
type GetApiOhlcvs       = QTimeFrame :> HAuthorization :> Get '[JSON, CSV] [ApiOhlcv]
type PatchApiOhlcv      = QTimeFrame :> HAuthorization :> ReqBody '[JSON, FormUrlEncoded] ApiOhlcv :> Patch '[JSON] NoContent

type WebApiServer
    = Get '[HTML] HomePage
 :<|> "public" :> Raw
 :<|> WebApi

-- |
-- web front 接続用 JSON API
type WebApi
    = "api" :> "v1" :> "exchange" :> "temporary" :> "code" :> CTempCode :> GetOAuthReply
 :<|> "api" :> "v1" :> "publish" :> "zmq" :> CMarketCode :> HAuthorization :> Put '[JSON] NoContent
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] ApiTypes.VerRev
 :<|> "api" :> "v1" :> "portfolios" :> GetApiPortfolios
 --
 :<|> "api" :> "v1" :> "stocks" :> "history" :> HAuthorization :> PostAccepted '[JSON] NoContent
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> GetApiOhlcvs
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> PatchApiOhlcv

--
--
webApiServer :: Config -> Servant.Server WebApiServer
webApiServer cnf =
    return HomePage
    :<|> Servant.serveDirectoryFileServer "elm-src/public"
    --
    :<|> exchangeTempCodeHandler cnf
    :<|> publishZmqHandler cnf
    :<|> versionHandler
    :<|> portfolioHandler cnf
    --
    :<|> updateHistoriesHandler cnf
    :<|> getHistoriesHandler cnf
    :<|> patchHistoriesHandler cnf

-- |
--
allowed :: Config -> AuthzValue -> Bool
allowed cnf value =
    value == T.unwords ["Bearer", token]
    where
    token = Conf.oauthAccessToken . Conf.slack $ cConf cnf

authzFailed :: Config -> AuthzValue -> Bool
authzFailed a b =
    not (allowed a b)

--
--
err400BadRequest :: Servant.Handler a
err400BadRequest = Servant.throwError Servant.err400

--
--
err401Unauthorized :: Servant.Handler a
err401Unauthorized = Servant.throwError Servant.err401

--
--
err500InternalServerError :: Servant.Handler a
err500InternalServerError = Servant.throwError Servant.err500

-- |
--
versionHandler :: Servant.Handler ApiTypes.VerRev
versionHandler =
    return ApiTypes.versionRevision

-- |
-- 仮コードをアクセストークンに交換する
exchangeTempCodeHandler :: Config
                        -> TempCode
                        -> Servant.Handler ApiTypes.OAuthReply
exchangeTempCodeHandler cnf tempCode =
    maybe err500InternalServerError go methodURI
    where
    --
    --
    go uri = do
        -- HTTPS接続ですよ
        manager <- M.liftIO $ N.newManager N.tlsManagerSettings
        -- Slack APIへアクセスする
        httpsResp <- M.liftIO $ BB.fetchHTTP manager [contentType] Nothing [] uri
        -- レスポンスボディにはJSON応答が入っている
        let json = N.responseBody httpsResp :: BL8.ByteString
        -- JSONをパースする
        let response = Aeson.eitherDecode json
        --
        -- デバッグ用にコンソールに出す
        --
        M.liftIO $ do
            print methodURI
            print json
            print response
        --
        --
        --
        maybe err401Unauthorized return $ pack response
    --
    --
    pack    :: Either String OAuthAccessResponse
            -> Maybe ApiTypes.OAuthReply
    pack (Right r)
        | respOk r = do
            accessToken <- respAccessToken r
            scope <- respScope r
            userName <- respUserName r
            Just ApiTypes.OAuthReply{..}
        | otherwise =
            Nothing
    pack (Left _) =
        Nothing
    --
    --
    methodURI   = URI.parseURI . TL.unpack . TLB.toLazyText $ "https://"
                <> "slack.com/api/oauth.access"
                <> "?" <> "client_id="      <> cID
                <> "&" <> "client_secret="  <> cSecret
                <> "&" <> "code="           <> TLB.fromString tempCode
    cID         = TLB.fromText . Conf.clientID . Conf.slack $ cConf cnf
    cSecret     = TLB.fromText . Conf.clientSecret . Conf.slack $ cConf cnf
    contentType = (Header.hContentType, "application/x-www-form-urlencoded")

-- |
--
updateHistoriesHandler :: Config -> AuthzValue -> Servant.Handler Servant.NoContent
updateHistoriesHandler cnf auth
    | authzFailed cnf auth = err401Unauthorized
    | otherwise = do
        M.liftIO . Agency.updateSomeRates $ cConf cnf
        return Servant.NoContent

-- |
--
publishZmqHandler :: Config -> MarketCode -> AuthzValue -> Servant.Handler Servant.NoContent
publishZmqHandler cnf code auth
    | authzFailed cnf auth = err401Unauthorized
    | otherwise =
        case Model.toTickerSymbol code of
            Nothing ->
                err400BadRequest
            Just ts -> do
                M.liftIO (STM.atomically . STM.writeTChan (cChan cnf) =<< prices ts)
                return Servant.NoContent
    where
    --
    --
    prices :: Model.TickerSymbol -> IO [ApiOhlcv]
    prices ts =
        flip DB.runSqlPersistMPool (cPool cnf) $ do
            xs <- DB.selectList
                    [Model.OhlcvTicker ==. ts]
                    [DB.Asc Model.OhlcvAt]
            return $ map (ApiTypes.toApiOhlcv. DB.entityVal) xs

-- |
--
portfolioHandler :: Config -> AuthzValue -> Servant.Handler [ApiPortfolio]
portfolioHandler cnf auth
    | authzFailed cnf auth = err401Unauthorized
    | otherwise =
        M.liftIO . flip DB.runSqlPersistMPool (cPool cnf) $ do
            xs <- DB.selectList [] [DB.Asc Model.PortfolioTicker]
            return $ map (ApiTypes.toApiPortfolio . DB.entityVal) xs

-- |
--
getHistoriesHandler :: Config -> MarketCode -> TimeFrame -> AuthzValue -> Servant.Handler [ApiOhlcv]
getHistoriesHandler cnf code timeFrame auth
    | authzFailed cnf auth = err401Unauthorized
    | otherwise = do
        M.liftIO $ print timeFrame
        case Model.toTickerSymbol code of
            Nothing ->
                err400BadRequest
            Just ticker ->
                M.liftIO . flip DB.runSqlPersistMPool (cPool cnf) $ do
                    xs <- DB.selectList
                            [ Model.OhlcvTf     ==. timeFrame
                            , Model.OhlcvTicker ==. ticker
                            ]
                            [DB.Asc Model.OhlcvAt]
                    return $ map (ApiTypes.toApiOhlcv . DB.entityVal) xs

-- |
-- 既存のデータを入れ替える(UPDATE)
patchHistoriesHandler :: Config -> MarketCode -> TimeFrame -> AuthzValue -> ApiOhlcv -> Servant.Handler Servant.NoContent
patchHistoriesHandler cnf codeStr timeFrame auth apiOhlcv
    | authzFailed cnf auth = err401Unauthorized
    | otherwise =
        let ts = Model.toTickerSymbol codeStr
            a = do
                x <- ts
                ApiTypes.fromApiOhlcv x timeFrame apiOhlcv
        in
        case (,) <$> ts <*> a of
            Nothing ->
                err400BadRequest
            Just (ticker, ohlcv) -> do
                go ticker ohlcv
                return Servant.NoContent
    where
    --
    --
    go ticker ohlcv =
        M.liftIO . flip DB.runSqlPersistMPool (cPool cnf) $
            -- 同じ時間の物があるか探してみる
            DB.selectFirst
                    [ Model.OhlcvTicker ==. ticker
                    , Model.OhlcvTf ==. timeFrame
                    , Model.OhlcvAt ==. Model.ohlcvAt ohlcv]
                    []
            >>= \case
                -- レコードが無かった場合は新規挿入する
                Nothing -> M.void $ DB.insert ohlcv
                -- レコードが有った場合は更新する
                Just (DB.Entity updKey _) ->
                    DB.update updKey
                        [ Model.OhlcvOpen     =. Model.ohlcvOpen ohlcv
                        , Model.OhlcvHigh     =. Model.ohlcvHigh ohlcv
                        , Model.OhlcvLow      =. Model.ohlcvLow ohlcv
                        , Model.OhlcvClose    =. Model.ohlcvClose ohlcv
                        , Model.OhlcvVolume   =. Model.ohlcvVolume ohlcv
                        , Model.OhlcvSource   =. Model.ohlcvSource ohlcv
                        ]

--
--
proxyWebApiServer :: Proxy WebApiServer
proxyWebApiServer = Proxy

--
--
app :: Config -> Servant.Application
app c =
    Servant.serve proxyWebApiServer $ webApiServer c

-- |
-- Web API サーバーを起動する
runWebServer :: Conf.Info -> ApiTypes.ServerTChan -> IO ()
runWebServer conf chan = do
    pool <- Logger.runStdoutLoggingT . runResourceT $ MySQL.createMySQLPool connInfo poolSize
    Warp.runSettings settings . app $ Config conf pool chan
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize = 8
    settings = Warp.setPort 8739 Warp.defaultSettings

