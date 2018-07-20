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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.WebServer
    ( runWebServer
    , ApiForFrontend
    , ApiForDocument
    ) where
import qualified Control.Concurrent.STM       as STM
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as Logger
import qualified Control.Monad.Trans          as MonadTrans
import           Control.Monad.Trans.Either   (EitherT, newEitherT, runEitherT)
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.CaseInsensitive         as CI
import qualified Data.Either                  as Either
import qualified Data.Maybe                   as Maybe
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
import           Servant                      ((:<|>) (..), (:>), Capture,
                                               Context (..), Delete, Get,
                                               Header, Header', Headers, JSON,
                                               NoContent, Patch, PostAccepted,
                                               Put, QueryParam, QueryParam',
                                               Raw, ReqBody, Required, Strict,
                                               Summary)
import qualified Servant
import           Servant.Auth.Server          (Auth)
import qualified Servant.Auth.Server          as Auth
import           Servant.CSV.Cassava          (CSV)
import qualified Servant.Docs
import qualified Servant.Elm
import           Servant.HTML.Lucid           (HTML)
import           System.IO.Temp               (withSystemTempDirectory)

import qualified BackOffice.Agency            as Agency
import qualified BrokerBackend                as BB
import qualified Conf
import           Model                        (TimeFrame (..))
import qualified Model
import           NetService.ApiTypes          (ApiOhlcv, ApiPortfolio,
                                               BearerToken (..),
                                               OAuthAccessResponse (..),
                                               OAuthReply (..), SVG,
                                               SvgBinary (..))
import qualified NetService.ApiTypes          as ApiTypes
import           NetService.HomePage          (HomePage (..))
import qualified NetService.PlotChart         as PlotChart

-- |
-- このサーバーで使う設定情報
data Config = Config
    { cVerRev :: ApiTypes.VerRev
    , cJWTS   :: Auth.JWTSettings
    , cConf   :: Conf.Info
    , cPool   :: MySQL.ConnectionPool
    , cChan   :: ApiTypes.ServerTChan
    }

newtype Chart = Chart SvgBinary
instance Servant.Docs.ToSample Chart where
    toSamples _ =
        Servant.Docs.noSamples

instance Servant.MimeRender SVG Chart where
    mimeRender ctype (Chart val) =
        render val
        where
        render :: SvgBinary -> BL8.ByteString
        render = Servant.mimeRender ctype

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
        let y = T.filter ('"' /=)$ URI.decodeText x
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
type QWidth = QueryParam "w" Int
type QHeight = QueryParam "h" Int

defQWidth :: Int
defQWidth = 500

defQHeight :: Int
defQHeight = 500

instance Servant.Docs.ToParam QWidth where
    toParam _ =
        Servant.Docs.DocQueryParam "w" ["int"] ("compose chart width. default is " ++ show defQWidth) Servant.Docs.Normal

instance Servant.Docs.ToParam QHeight where
    toParam _ =
        Servant.Docs.DocQueryParam "h" ["int"] ("compose chart height. default is " ++ show defQHeight) Servant.Docs.Normal
-- |
--
type ChartWithCacheControl = Headers '[Header "Cache-Conrol" T.Text] Chart

instance Servant.Docs.ToSample T.Text where
    toSamples _ =
        Servant.Docs.singleSample "no-store"

-- |
--
type AuthzValue     = T.Text
type HAuthorization = Header' '[Required, Strict] "Authorization" AuthzValue

type ApiForFrontend
    = Summary "publish price on the zeroMQ infrastructure."
        :> "api" :> "v1" :> "publish" :> "zmq" :> CMarketCode :> HAuthorization :> Put '[JSON] NoContent
 :<|> Summary "This endpoint runs the crawler."
        :> "api" :> "v1" :> "stocks" :> "history" :> "all" :> PostAccepted '[JSON] NoContent
 :<|> Summary "Select prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> HAuthorization :> Get '[JSON, CSV] [ApiOhlcv]
 :<|> Summary "Insert prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> HAuthorization :> ReqBody '[JSON, CSV] [ApiOhlcv] :> Put '[JSON] [ApiOhlcv]
 :<|> Summary "Update / Insert prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> HAuthorization :> ReqBody '[JSON] [ApiOhlcv] :> Patch '[JSON] [ApiOhlcv]
 :<|> Summary "Delete prices"
        :> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> HAuthorization :> ReqBody '[JSON] [ApiOhlcv] :> Delete '[JSON] [ApiOhlcv]
 :<|> "api" :> "v1" :> "exchange" :> "temporary" :> "code" :> CTempCode :> Get '[JSON] BearerToken
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] ApiTypes.VerRev
 :<|> "api" :> "v1" :> "portfolios" :> Get '[JSON] [ApiPortfolio]

type ApiForDocument
    = ApiForFrontend
 :<|> Summary "Get Chart, suffix is only .svg"
       :> "api" :> "v1" :> "stocks" :> "chart" :> CMarketCode :> QTimeFrame :> QWidth :> QHeight :> Get '[SVG] ChartWithCacheControl

-- |
--
type Protected
    = "api" :> "v1" :> "publish" :> "zmq" :> CMarketCode :> Put '[JSON] NoContent
 :<|> "api" :> "v1" :> "stocks" :> "history" :> "all" :> PostAccepted '[JSON] NoContent
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> Get '[JSON, CSV] [ApiOhlcv]
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> ReqBody '[JSON, CSV] [ApiOhlcv] :> Put '[JSON] [ApiOhlcv]
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> ReqBody '[JSON] [ApiOhlcv] :> Patch '[JSON] [ApiOhlcv]
 :<|> "api" :> "v1" :> "stocks" :> "history" :> CMarketCode :> QTimeFrame :> ReqBody '[JSON] [ApiOhlcv] :> Delete '[JSON] [ApiOhlcv]

-- |
--
type Unprotected
    = Get '[HTML] HomePage
 :<|> "public" :> Raw
 :<|> "api" :> "v1" :> "exchange" :> "temporary" :> "code" :> CTempCode :> Get '[JSON] BearerToken
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] ApiTypes.VerRev
 :<|> "api" :> "v1" :> "portfolios" :> Get '[JSON] [ApiPortfolio]
 :<|> "api" :> "v1" :> "stocks" :> "chart" :> CMarketCode :> QTimeFrame :> QWidth :> QHeight :> Get '[SVG] ChartWithCacheControl

-- |
--
protected :: Config -> Auth.AuthResult OAuthReply-> Servant.Server Protected
protected cnf result =
    case result of
    Auth.Authenticated _ ->
        publishZmqHandler cnf
        :<|> updateHistoriesHandler cnf
        :<|> getHistoriesHandler cnf
        :<|> putHistoriesHandler cnf
        :<|> patchHistoriesHandler cnf
        :<|> deleteHistoriesHandler cnf
    Auth.BadPassword ->
        Auth.throwAll Servant.err401 { Servant.errBody = "bad password" }
    Auth.NoSuchUser ->
        Auth.throwAll Servant.err401 { Servant.errBody = "no such user" }
    Auth.Indefinite ->
        Auth.throwAll Servant.err401 { Servant.errBody = "indefinite" }

-- |
--
unprotected :: Config -> Servant.Server Unprotected
unprotected cnf
    = return (HomePage clientID)
    :<|> Servant.serveDirectoryFileServer "elm-src/public"
    :<|> exchangeTempCodeHandler cnf
    :<|> return (cVerRev cnf)
    :<|> getPortfolioHandler cnf
    :<|> getChartHandler cnf
    where
    clientID = Conf.clientID . Conf.slack . cConf $ cnf

-- |
--
type WebApi = (Auth '[Auth.JWT] OAuthReply:> Protected) :<|> Unprotected

-- |
--
webApiServer :: Config -> Servant.Server WebApi
webApiServer cfg = protected cfg :<|> unprotected cfg

-- |
-- Web API サーバーを起動する
runWebServer :: ApiTypes.VerRev -> Conf.Info -> ApiTypes.ServerTChan -> IO ()
runWebServer versionRevision conf chan = do
    pool <- Logger.runStdoutLoggingT . runResourceT $ createPool
    myKey <- Auth.generateKey
    let api = Proxy :: Proxy WebApi
        jwtCfg = Auth.defaultJWTSettings myKey
        context = jwtCfg :. Auth.defaultCookieSettings :. Servant.EmptyContext
        server = webApiServer (Config versionRevision jwtCfg conf pool chan)
    Warp.runSettings settings (Servant.serveWithContext api context server)
    where
    --
    --
    createPool = MySQL.createMySQLPool connInfo poolSize
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize = 8
    settings = Warp.setPort 8739 Warp.defaultSettings

--
--
err400BadRequest :: BL8.ByteString -> Servant.Handler a
err400BadRequest x = Servant.throwError Servant.err400 { Servant.errBody = x }

--
--
err401Unauthorized :: Servant.Handler a
err401Unauthorized = Servant.throwError Servant.err401

--
--
err404NotFound :: Servant.Handler a
err404NotFound = Servant.throwError Servant.err404

--
--
err409Conflict :: BL8.ByteString -> Servant.Handler a
err409Conflict x = Servant.throwError Servant.err409 { Servant.errBody = x }

--
--
err500InternalServerError :: Servant.Handler a
err500InternalServerError = Servant.throwError Servant.err500

-- |
-- 仮コードをアクセストークンに交換する
exchangeTempCodeHandler :: Config -> TempCode -> Servant.Handler BearerToken
exchangeTempCodeHandler cnf tempCode = do
    uri <- maybe err500InternalServerError pure methodURI
    unverifiedReply <- takeOAuthReply <$> doExchange uri
    case unverifiedReply of
        Just genuineReply | verifyCredentials genuineReply -> do
            jwt <- makeJWT genuineReply
            M.liftIO . putStrLn . show $ jwt
            pure . BearerToken . T.pack . BL8.unpack $ jwt
        _ ->
            err401Unauthorized
    where
    --
    --
    doExchange uri = M.liftIO $ do
        -- HTTPS接続ですよ
        manager <- N.newManager N.tlsManagerSettings
        -- Slack APIへアクセスする
        httpsResp <- BB.fetchHTTP manager [contentType] Nothing [] uri
        -- レスポンスボディにはJSON応答が入っている
        let json = N.responseBody httpsResp :: BL8.ByteString
        -- JSONをパースする
        return $ Aeson.eitherDecode json
    --
    --
    takeOAuthReply :: Either a ApiTypes.OAuthAccessResponse -> Maybe ApiTypes.OAuthReply
    takeOAuthReply x = do
        y <- either (const Nothing) Just x
        M.guard (respOk y)
        accessToken <- respAccessToken y
        scope <- respScope y
        userName <- respUserName y
        Just ApiTypes.OAuthReply{..}
    --
    --
    makeJWT :: ApiTypes.OAuthReply -> Servant.Handler BL8.ByteString
    makeJWT oauthrep =
        either err ok =<< M.liftIO (Auth.makeJWT oauthrep settings Nothing)
        where
        settings = cJWTS cnf
        err _ = err401Unauthorized
        ok = pure
    --
    --
    verifyCredentials :: ApiTypes.OAuthReply -> Bool
    verifyCredentials ApiTypes.OAuthReply{..} =
        (Conf.oauthAccessToken . Conf.slack $ cConf cnf) == accessToken
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
updateHistoriesHandler :: Config -> Servant.Handler Servant.NoContent
updateHistoriesHandler cnf = do
    M.liftIO . Agency.updateSomeRates $ cConf cnf
    return Servant.NoContent

-- |
--
publishZmqHandler :: Config -> MarketCode -> Servant.Handler Servant.NoContent
publishZmqHandler cnf codeStr =
    case Model.toTickerSymbol codeStr of
        Nothing ->
            err400BadRequest . BL8.pack $ unwords ["market code", codeStr, "is unknown"]
        Just ts -> do
            M.liftIO (STM.atomically . STM.writeTChan (cChan cnf) =<< prices ts)
            return Servant.NoContent
    where
    --
    --
    prices :: Model.TickerSymbol -> IO [ApiOhlcv]
    prices ts =
        map (ApiTypes.toApiOhlcv . DB.entityVal) <$> selectList ts
    --
    --
    selectList ticker =
        flip DB.runSqlPersistMPool (cPool cnf) $
            DB.selectList   [Model.OhlcvTicker ==. ticker]
                            [DB.Asc Model.OhlcvAt]

-- |
--
getPortfolioHandler :: Config -> Servant.Handler [ApiPortfolio]
getPortfolioHandler cnf =
    map (ApiTypes.toApiPortfolio . DB.entityVal) <$> M.liftIO selectList
    where
    --
    --
    selectList =
        flip DB.runSqlPersistMPool (cPool cnf) $
            DB.selectList [] [DB.Asc Model.PortfolioTicker]

-- |
--
getHistoriesHandler :: Config -> MarketCode -> TimeFrame -> Servant.Handler [ApiOhlcv]
getHistoriesHandler cnf codeStr timeFrame =
    go $ Model.toTickerSymbol codeStr
    where
    --
    --
    go Nothing =
        err400BadRequest . BL8.pack $ unwords ["market code", codeStr, "is unknown"]
    --
    --
    go (Just ticker) =
        map (ApiTypes.toApiOhlcv . DB.entityVal) <$> M.liftIO (selectList ticker)
    --
    --
    selectList ticker =
        flip DB.runSqlPersistMPool (cPool cnf) $
            DB.selectList   [ Model.OhlcvTf     ==. timeFrame
                            , Model.OhlcvTicker ==. ticker
                            ]
                            [DB.Asc Model.OhlcvAt]

-- |
-- 新規挿入(INSERT)
putHistoriesHandler :: Config -> MarketCode -> TimeFrame -> [ApiOhlcv] -> Servant.Handler [ApiOhlcv]
putHistoriesHandler cnf codeStr timeFrame apiOhlcvs =
    go $ Model.toTickerSymbol codeStr
    where
    --
    --
    go :: Maybe Model.TickerSymbol -> Servant.Handler [ApiOhlcv]
    go Nothing =
        err400BadRequest . BL8.pack $ unwords ["market code", codeStr, "is unknown"]
    go (Just ticker) = do
        responce <- M.mapM (M.liftIO . runEitherT . insert ticker) apiOhlcvs
        case (Either.lefts responce, Either.rights responce) of
            ([], rights) ->
                -- 成功
                return rights
            (lefts, _) ->
                -- 部分的成功または失敗
                err409Conflict (Aeson.encode lefts)
    --
    --
    insert :: Model.TickerSymbol -> ApiOhlcv -> EitherT ApiOhlcv IO ApiOhlcv
    insert ticker apiOhlcv =
        let err _ = newEitherT . return $ Left apiOhlcv
            ok _  = newEitherT . return $ Right apiOhlcv
        in
        case ApiTypes.fromApiOhlcv ticker timeFrame apiOhlcv of
            Left x ->
                err x
            Right ohlcv -> do
                responce <- MonadTrans.lift $ insertOhlcv (cPool cnf) ohlcv
                either err ok responce

-- |
-- データを入れる(INSERT)
insertOhlcv :: MySQL.ConnectionPool -> Model.Ohlcv -> IO (Either () ())
insertOhlcv pool ohlcv@Model.Ohlcv{..} =
    flip DB.runSqlPersistMPool pool $ do
        -- 同じ時間の物があるか探してみる
        exists <- Maybe.isJust <$> DB.selectFirst
                [ Model.OhlcvTicker ==. ohlcvTicker
                , Model.OhlcvTf ==. ohlcvTf
                , Model.OhlcvAt ==. ohlcvAt
                ]
                []
        -- 無かった場合は新規挿入, 有った場合は何もしない
        if exists
        then
            return $ Left ()
        else do
            M.void $ DB.insert ohlcv
            return $ Right ()

--- |
-- 既存のデータを入れ替える(UPDATE / INSERT)
patchHistoriesHandler :: Config -> MarketCode -> TimeFrame -> [ApiOhlcv] -> Servant.Handler [ApiOhlcv]
patchHistoriesHandler cnf codeStr timeFrame apiOhlcvs =
    go $ Model.toTickerSymbol codeStr
    where
    --
    --
    go :: Maybe Model.TickerSymbol -> Servant.Handler [ApiOhlcv]
    go Nothing =
        err400BadRequest . BL8.pack $ unwords ["market code", codeStr, "is unknown"]
    go (Just ticker) =
        Maybe.catMaybes <$> M.mapM (M.liftIO . runMaybeT . store ticker) apiOhlcvs
    --
    --
    store :: Model.TickerSymbol -> ApiOhlcv -> MaybeT IO ApiOhlcv
    store ticker apiOhlcv =
        case ApiTypes.fromApiOhlcv ticker timeFrame apiOhlcv of
            Left _ ->
                MaybeT (return Nothing)
            Right val -> do
                MonadTrans.lift (insertOrUpdateOhlcv (cPool cnf) val)
                MaybeT (return $ Just apiOhlcv)

-- |
-- データを入れる
-- もしくは
-- 既存のデータを入れ替える(UPDATE / INSERT)
insertOrUpdateOhlcv :: MySQL.ConnectionPool -> Model.Ohlcv -> IO ()
insertOrUpdateOhlcv pool ohlcv@Model.Ohlcv{..} =
    flip DB.runSqlPersistMPool pool $ do
        -- 同じ時間の物があるか探してみる
        entity <- DB.selectFirst
                    [ Model.OhlcvTicker ==. ohlcvTicker
                    , Model.OhlcvTf ==. ohlcvTf
                    , Model.OhlcvAt ==. ohlcvAt
                    ]
                    []
        case DB.entityKey <$> entity of
            -- レコードが無かった場合は新規挿入する
            Nothing ->
                M.void $ DB.insert ohlcv
            -- レコードが有った場合は更新する
            Just key ->
                DB.update key
                    [ Model.OhlcvOpen     =. ohlcvOpen
                    , Model.OhlcvHigh     =. ohlcvHigh
                    , Model.OhlcvLow      =. ohlcvLow
                    , Model.OhlcvClose    =. ohlcvClose
                    , Model.OhlcvVolume   =. ohlcvVolume
                    , Model.OhlcvSource   =. ohlcvSource
                    ]

-- |
-- 既存のデータを削除する(DELETE)
deleteHistoriesHandler :: Config -> MarketCode -> TimeFrame -> [ApiOhlcv] -> Servant.Handler [ApiOhlcv]
deleteHistoriesHandler cnf codeStr timeFrame apiOhlcvs =
    go $ Model.toTickerSymbol codeStr
    where
    --
    --
    go :: Maybe Model.TickerSymbol -> Servant.Handler [ApiOhlcv]
    go Nothing =
        err400BadRequest . BL8.pack $ unwords ["market code", codeStr, "is unknown"]
    go (Just ticker) =
        Maybe.catMaybes <$> M.mapM (M.liftIO . runMaybeT . delete ticker) apiOhlcvs
    --
    --
    delete :: Model.TickerSymbol -> ApiOhlcv -> MaybeT IO ApiOhlcv
    delete ticker apiOhlcv =
        case ApiTypes.fromApiOhlcv ticker timeFrame apiOhlcv of
            Left _ ->
                MaybeT (return Nothing)
            Right ohlcv -> do
                let err _ = MaybeT $ return Nothing
                let ok _  = MaybeT $ return (Just apiOhlcv)
                result <- MonadTrans.lift $ deleteOhlcv (cPool cnf) ohlcv
                either err ok result

-- |
-- データを削除する(DELETE)
deleteOhlcv :: MySQL.ConnectionPool -> Model.Ohlcv -> IO (Either () ())
deleteOhlcv pool Model.Ohlcv{..} =
    flip DB.runSqlPersistMPool pool $ do
        -- 同じ物があるか探してみる
        entity <- DB.selectFirst
                    [ Model.OhlcvTicker ==. ohlcvTicker
                    , Model.OhlcvTf     ==. ohlcvTf
                    , Model.OhlcvAt     ==. ohlcvAt
                    , Model.OhlcvOpen   ==. ohlcvOpen
                    , Model.OhlcvHigh   ==. ohlcvHigh
                    , Model.OhlcvLow    ==. ohlcvLow
                    , Model.OhlcvClose  ==. ohlcvClose
                    , Model.OhlcvVolume ==. ohlcvVolume
                    , Model.OhlcvSource ==. ohlcvSource
                    ]
                    []
        case DB.entityKey <$> entity of
            -- 無かった場合は何もしない
            Nothing ->
                return $ Left ()
            -- 有った場合は削除
            Just key -> do
                DB.deleteWhere [ Model.OhlcvId ==. key ]
                return $ Right ()

-- |
--
getChartHandler :: Config -> MarketCode -> TimeFrame -> Maybe Int -> Maybe Int -> Servant.Handler ChartWithCacheControl
getChartHandler cnf codeStr timeFrame qWidth qHeight =
    case Model.toTickerSymbol codeBody of
        Nothing ->
            err400BadRequest . BL8.pack $ unwords ["market code", codeStr, "is unknown"]
        Just ticker
            | CI.mk codeSuffix == ".svg" ->
                go ticker
            | otherwise ->
                err404NotFound
    where
    --
    --
    (codeBody, codeSuffix) = span (/= '.') codeStr
    --
    --
    chartSize =
        ( Maybe.fromMaybe defQWidth qWidth
        , Maybe.fromMaybe defQHeight qHeight
        )
    --
    --
    chartSVG :: PlotChart.ChartData -> Servant.Handler SvgBinary
    chartSVG chartData =
        M.liftIO . withSystemTempDirectory "tractor" $ \fpath -> do
            let fname = fpath ++ "/plot.svg"
            M.void $ PlotChart.plotSVG fname chartData
            SvgBinary <$> BL8.readFile fname
    --
    --
    go ticker = do
        ohlcvs <- reverse . map DB.entityVal <$> M.liftIO (selectList ticker)
        let chartData = PlotChart.ChartData
                            { cSize = chartSize
                            , cTitle = "stock prices"
                            , cOhlcvs = ohlcvs
                            }
        Servant.addHeader "private, no-store, no-cache, must-revalidate" . Chart <$> chartSVG chartData
    --
    --
    selectList ticker =
        flip DB.runSqlPersistMPool (cPool cnf) $
            DB.selectList   [ Model.OhlcvTf     ==. timeFrame
                            , Model.OhlcvTicker ==. ticker
                            ]
                            [DB.Desc Model.OhlcvAt
                            ,DB.LimitTo 100]

