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
    , WebApi
    , proxyWebApiServer
    ) where
import qualified Control.Concurrent.STM       as STM
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as Logger
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson                   ((.:), (.:?))
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy.Char8   as BL8
import           Data.Monoid                  ((<>))
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import           Database.Persist             ((==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import           Lucid                        (body_, charset_, content_,
                                               doctype_, head_, href_, html_,
                                               lang_, link_, meta_, name_, rel_,
                                               script_, src_, title_, type_)
import qualified Lucid
import qualified Network.HTTP.Conduit         as N
import qualified Network.HTTP.Types.Header    as Header
import qualified Network.URI                  as URI
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Servant
import           Servant.API                  ((:<|>) (..), (:>), Capture, Get,
                                               Header', JSON, NoContent, Post,
                                               Put, Raw, Required, Strict)
import           Servant.CSV.Cassava          (CSV)
import qualified Servant.Docs
import qualified Servant.Elm
import           Servant.HTML.Lucid           (HTML)

import qualified BackOffice.Agency            as Agency
import qualified BrokerBackend                as BB
import qualified Conf
import qualified Model
import qualified NetService.ApiTypes          as ApiTypes
import           VerRev                       (VerRev)
import qualified VerRev


instance Servant.Elm.ElmType VerRev
instance Servant.Docs.ToSample VerRev where
    toSamples _ =
        Servant.Docs.singleSample VerRev.versionRevision

data Config = Config
    { cConf :: Conf.Info
    , cPool :: MySQL.ConnectionPool
    , cChan :: ApiTypes.ServerTChan
    }

-- |
-- slack api oauth.accessからのJSON返答
data OAuthAccessResponse = OAuthAccessResponse
    { respOk          :: Bool
    , respError       :: Maybe T.Text
    , respAccessToken :: Maybe T.Text
    , respScope       :: Maybe T.Text
    , respUserId      :: Maybe T.Text
    , respUserName    :: Maybe T.Text
    , respTeamId      :: Maybe T.Text
    } deriving Show

instance Aeson.FromJSON OAuthAccessResponse where
    parseJSON = Aeson.withObject "oauth.access response" $ \o -> do
        respOk          <- o .: "ok"
        respError       <- o .:? "error"
        respAccessToken <- o .:? "access_token"
        respScope       <- o .:? "scope"
        --
        -- userの中のid,nameを取り出す
        oUser           <- o .:? "user"
        respUserId      <- maybe (pure Nothing) (.:? "id") oUser
        respUserName    <- maybe (pure Nothing) (.:? "name") oUser
        --
        -- teamの中のidを取り出す
        oTeam           <- o .:? "team"
        respTeamId      <- maybe (pure Nothing) (.:? "id") oTeam
        return OAuthAccessResponse{..}

-- |
-- アプリケーションのホームページ
data HomePage = HomePage
instance Lucid.ToHtml HomePage where
    --
    --
    toHtml _ = do
        doctype_
        html_ [lang_ "ja"] $ do
            head_ $ do
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                title_ $ Lucid.toHtmlRaw ("Dashboard &#8212; TRACTOR" :: T.Text)
                link_ [rel_ "stylesheet", type_ "text/css", href_ "https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext"]
                link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/icon?family=Material+Icons"]
                link_ [rel_ "stylesheet", href_ "https://code.getmdl.io/1.3.0/material.blue_grey-lime.min.css"]
                link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Gugi"]
                --
                script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/dialog-polyfill/0.4.4/dialog-polyfill.min.js"] T.empty
                link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdnjs.cloudflare.com/ajax/libs/dialog-polyfill/0.4.4/dialog-polyfill.min.css"]
                --
                script_ [src_ "https://cdn.polyfill.io/v2/polyfill.js?features=Event.focusin"] T.empty
                --
            body_ $ do
                script_ [src_ "public/main.js"] T.empty
                script_ [] ("app = Elm.Main.fullscreen();" :: T.Text)
    --
    --
    toHtmlRaw = Lucid.toHtml

instance Servant.Docs.ToSample HomePage where
    toSamples _ = Servant.Docs.singleSample HomePage

--
-- for servant doc
instance Servant.Docs.ToCapture (Capture "marketCode" T.Text) where
    toCapture _ =
        Servant.Docs.DocCapture
        "market code"
        "NI225, TOPIX, TYO8306 etc..."

instance Servant.Docs.ToCapture (Capture "tempCode" T.Text) where
    toCapture _ =
        Servant.Docs.DocCapture "tempCode" "Exchanging a temporary code for an access token"

type WebApiServer
    = Get '[HTML] HomePage
 :<|> "public" :> Raw
 :<|> WebApi

-- |
--
type AuthHeaderValue = T.Text
type AuthHeader = Header' '[Required, Strict] "Authorization" AuthHeaderValue

-- |
-- web front 接続用 JSON API
--
type WebApi
    = "api" :> "v1" :> "exchange" :> "temporary" :> "code" :> Capture "tempCode" T.Text :> Get '[JSON] ApiTypes.OAuthReply
 :<|> "api" :> "v1" :> "publish" :> "zmq" :> Capture "marketCode" T.Text :> AuthHeader :> Put '[JSON] NoContent
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] VerRev
 :<|> "api" :> "v1" :> "portfolios" :> AuthHeader :> Get '[JSON] [ApiTypes.Portfolio]
 :<|> "api" :> "v1" :> "quotes" :> "all" :> "update" :> AuthHeader :> Post '[JSON] NoContent
 :<|> "api" :> "v1" :> "quotes" :> Capture "marketCode" T.Text :> AuthHeader :> Get '[JSON, CSV] [ApiTypes.Ohlcv]

--
--
webApiServer :: Config -> Servant.Server WebApiServer
webApiServer cnf =
    return HomePage
    :<|> Servant.serveDirectoryFileServer "elm-src/public"
    --
    :<|> exchangeTempCode
    :<|> publishZmq
    :<|> version
    :<|> portfolios
    :<|> updateAll
    :<|> histories
    where
    exchangeTempCode = exchangeTempCodeHandler cnf
    publishZmq = publishZmqHandler cnf
    version = versionHandler
    portfolios = portfolioHandler cnf
    updateAll = updateAllHandler cnf
    histories = historiesHandler cnf

-- |
--
versionHandler :: Servant.Handler VerRev
versionHandler =
    return VerRev.versionRevision

-- |
--
isMatchBearerToken :: AuthHeaderValue -> Config -> Bool
isMatchBearerToken value cnf =
    value == T.concat ["Bearer ", token]
    where
    token = Conf.oauthAccessToken . Conf.slack $ cConf cnf

-- |
-- 仮コードをアクセストークンに交換する
exchangeTempCodeHandler :: Config
                        -> T.Text
                        -> Servant.Handler ApiTypes.OAuthReply
exchangeTempCodeHandler cnf tempCode =
    maybe err500 go methodURI
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
        maybe err401 return $ pack response
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
    err401 = Servant.throwError Servant.err401
    err500 = Servant.throwError Servant.err500
    --
    --
    methodURI   = URI.parseURI . TL.unpack . TLB.toLazyText $ "https://"
                <> "slack.com/api/oauth.access"
                <> "?" <> "client_id="      <> cID
                <> "&" <> "client_secret="  <> cSecret
                <> "&" <> "code="           <> TLB.fromText tempCode
    cID         = TLB.fromText . Conf.clientID . Conf.slack $ cConf cnf
    cSecret     = TLB.fromText . Conf.clientSecret . Conf.slack $ cConf cnf
    contentType = (Header.hContentType, "application/x-www-form-urlencoded")

-- |
--
updateAllHandler    :: Config
                    -> AuthHeaderValue
                    -> Servant.Handler Servant.NoContent
updateAllHandler cnf auth
    | isMatchBearerToken auth cnf = go
    | otherwise = err401
    where
    go = do
        M.liftIO . Agency.updateSomeRates $ cConf cnf
        return Servant.NoContent
    err401 = do
        M.liftIO $ print auth
        Servant.throwError Servant.err401

-- |
--
toTickerSymbol :: T.Text -> Maybe Model.TickerSymbol
toTickerSymbol = Model.toTickerSymbol . T.unpack

-- |
--
publishZmqHandler   :: Config
                    -> T.Text
                    -> AuthHeaderValue
                    -> Servant.Handler Servant.NoContent
publishZmqHandler cnf code auth
    | isMatchBearerToken auth cnf = maybe err400 go (toTickerSymbol code)
    | otherwise = err401
    where
    go ts = do
        M.liftIO (STM.atomically . STM.writeTChan (cChan cnf) =<< prices ts)
        return Servant.NoContent
    err400 = Servant.throwError Servant.err400
    err401 = do
        M.liftIO $ print auth
        Servant.throwError Servant.err401
    --
    --
    prices :: Model.TickerSymbol -> IO [ApiTypes.Ohlcv]
    prices ts =
        flip DB.runSqlPersistMPool (cPool cnf) $
            fromInternal<$> DB.selectList [Model.OhlcvTicker ==. ts] [DB.Asc Model.OhlcvAt]
    --
    --
    fromInternal:: [DB.Entity Model.Ohlcv] -> [ApiTypes.Ohlcv]
    fromInternal= map (ApiTypes.toOhlcv. DB.entityVal)

-- |
--
portfolioHandler    :: Config
                    -> AuthHeaderValue
                    -> Servant.Handler [ApiTypes.Portfolio]
portfolioHandler cnf auth
    | isMatchBearerToken auth cnf = M.liftIO getPortfolio
    | otherwise = err401
    where
    err401 = do
        M.liftIO $ print auth
        Servant.throwError Servant.err401
    --
    --
    getPortfolio :: IO [ApiTypes.Portfolio]
    getPortfolio =
        flip DB.runSqlPersistMPool (cPool cnf) $
            fromInternal <$> DB.selectList [] [DB.Asc Model.PortfolioTicker]
    --
    --
    fromInternal :: [DB.Entity Model.Portfolio] -> [ApiTypes.Portfolio]
    fromInternal = map (ApiTypes.toPortfolio . DB.entityVal)

-- |
--
historiesHandler    :: Config
                    -> T.Text
                    -> AuthHeaderValue
                    -> Servant.Handler [ApiTypes.Ohlcv]
historiesHandler cnf code auth
    | isMatchBearerToken auth cnf = maybe err400 go (toTickerSymbol code)
    | otherwise = err401
    where
    go = M.liftIO . takePrices (cPool cnf)
    err400 = Servant.throwError Servant.err400
    err401 = do
        M.liftIO $ print auth
        Servant.throwError Servant.err401

--
--
takePrices :: MySQL.ConnectionPool -> Model.TickerSymbol -> IO [ApiTypes.Ohlcv]
takePrices pool ticker =
    flip DB.runSqlPersistMPool pool $
       fromInternal <$> DB.selectList [Model.OhlcvTicker ==. ticker] [DB.Asc Model.OhlcvAt]
    where
    --
    --
    fromInternal:: [DB.Entity Model.Ohlcv] -> [ApiTypes.Ohlcv]
    fromInternal= map (ApiTypes.toOhlcv . DB.entityVal)

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
    pool <- Logger.runNoLoggingT . runResourceT $ MySQL.createMySQLPool connInfo poolSize
    Warp.runSettings settings . app $ Config conf pool chan
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize = 8
    settings = Warp.setPort 8739 Warp.defaultSettings

