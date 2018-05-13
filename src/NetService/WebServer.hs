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

REST API サーバーモジュールです
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.WebServer
    ( runWebServer
    , WebApi
    , proxyWebApi
    ) where
import qualified Control.Concurrent.STM       as STM
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Aeson                   as Aeson
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import           Database.Persist             ((==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified GHC.Conc                     as Conc
import           GHC.Generics                 (Generic)
import qualified GHC.Stats                    as Stats
import           Lucid                        (body_, charset_, content_,
                                               doctype_, head_, href_, html_,
                                               lang_, link_, meta_, name_, rel_,
                                               script_, src_, title_, type_)
import qualified Lucid
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Servant
import           Servant.API                  ((:<|>) (..), (:>))
import           Servant.API                  (Capture, Get, JSON, NoContent,
                                               Post, Put, Raw)
import           Servant.CSV.Cassava          (CSV)
import qualified Servant.Docs
import qualified Servant.Elm
import           Servant.HTML.Lucid           (HTML)
import qualified System.Mem                   as Mem

import qualified BackOffice.Agency            as Agency
import qualified Conf
import qualified Model
import qualified NetService.ApiTypes          as ApiTypes
import           VerRev                       (VerRev)
import qualified VerRev


instance Servant.Elm.ElmType VerRev
instance Servant.Docs.ToSample VerRev where
    toSamples _ =
        Servant.Docs.singleSample VerRev.versionRevision

deriving instance Generic Stats.GCDetails
instance Aeson.FromJSON Stats.GCDetails
instance Aeson.ToJSON Stats.GCDetails

deriving instance Generic Stats.RTSStats
instance Aeson.FromJSON Stats.RTSStats
instance Aeson.ToJSON Stats.RTSStats

data Health = Health
    { hNumCapabilities :: Int
    , hNumProcessors   :: Int
    , hNumSparks       :: Int
    , hStats           :: Stats.RTSStats
    } deriving Generic
instance Aeson.FromJSON Health
instance Aeson.ToJSON Health
instance Servant.Docs.ToSample Health where
    toSamples _ = Servant.Docs.noSamples

data Config = Config
    { cConf :: Conf.Info
    , cPool :: MySQL.ConnectionPool
    , cChan :: ApiTypes.ServerTChan
    }

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

--
-- for servant doc
instance Servant.Docs.ToSample HomePage where
    toSamples _ = Servant.Docs.singleSample HomePage

instance Servant.Docs.ToCapture (Capture "code" String) where
    toCapture _ =
        Servant.Docs.DocCapture
        "market code"
        "NI225, TOPIX, TYO8306 etc..."

-- |
-- web front 接続用 JSON API
type WebApi
    = "api" :> "v1" :> "publish" :> "zmq" :> Capture "code" String :> Put '[JSON] NoContent
 :<|> "api" :> "v1" :> "version" :> Get '[JSON] VerRev
 :<|> "api" :> "v1" :> "portfolios" :> Get '[JSON] [ApiTypes.Portfolio]
 :<|> "api" :> "v1" :> "quotes" :> "all" :> "update" :> Post '[JSON] NoContent
 :<|> "api" :> "v1" :> "quotes" :> Capture "code" String :> Get '[JSON, CSV] [ApiTypes.Ohlcv]

type WebApi'
 = Get '[HTML] HomePage
 :<|> "public" :> Raw
 :<|> "api" :> "v1" :> "health" :> Get '[JSON] Health
 :<|> WebApi

--
--
webApiServer :: Config -> Servant.Server WebApi'
webApiServer cnf =
    home
    :<|> Servant.serveDirectoryFileServer "elm-src/public"
    :<|> health
    --
    :<|> publishZmq
    :<|> version
    :<|> portfolios
    --
    :<|> updateAll :<|> histories
    where
    home = return HomePage
    publishZmq = publishZmqHandler (cPool cnf) (cChan cnf)
    health = healthHandler
    version = versionHandler
    portfolios = portfolioHandler (cPool cnf)
    updateAll = updateAllHandler cnf
    histories = historiesHandler (cPool cnf)

-- |
--
updateAllHandler :: Config -> Servant.Handler Servant.NoContent
updateAllHandler cnf = do
    M.liftIO . Agency.updateSomeRates $ cConf cnf
    return Servant.NoContent

-- |
--
publishZmqHandler :: MySQL.ConnectionPool -> ApiTypes.ServerTChan -> String -> Servant.Handler Servant.NoContent
publishZmqHandler pool chan codeStr =
    case Model.toTickerSymbol codeStr of
        Just ts -> do
            ps <- M.liftIO $ prices ts
            M.liftIO . STM.atomically $ STM.writeTChan chan ps
            return Servant.NoContent
        Nothing -> Servant.throwError Servant.err400 { Servant.errBody = "error" }
    where
    --
    --
    prices :: Model.TickerSymbol -> IO [ApiTypes.Ohlcv]
    prices ticker =
        flip DB.runSqlPersistMPool pool $
            fromInternal<$> DB.selectList [Model.OhlcvTicker ==. ticker] [DB.Asc Model.OhlcvAt]
    --
    --
    fromInternal:: [DB.Entity Model.Ohlcv] -> [ApiTypes.Ohlcv]
    fromInternal= map (ApiTypes.toOhlcv. DB.entityVal)

-- |
--
healthHandler :: Servant.Handler Health
healthHandler = do
    M.liftIO Mem.performGC
    sts <- M.liftIO Stats.getRTSStats
    c <- M.liftIO Conc.getNumCapabilities
    p <- M.liftIO Conc.getNumProcessors
    s <- M.liftIO Conc.numSparks
    return Health
        { hNumCapabilities = c
        , hNumProcessors = p
        , hNumSparks = s
        , hStats = sts
        }

-- |
--
versionHandler :: Servant.Handler VerRev
versionHandler =
    return VerRev.versionRevision

-- |
--
portfolioHandler :: MySQL.ConnectionPool -> Servant.Handler [ApiTypes.Portfolio]
portfolioHandler pool =
    M.liftIO pf
    where
    --
    --
    pf :: IO [ApiTypes.Portfolio]
    pf =
        flip DB.runSqlPersistMPool pool $
            fromInternal <$> DB.selectList [] [DB.Asc Model.PortfolioTicker]
    --
    --
    fromInternal :: [DB.Entity Model.Portfolio] -> [ApiTypes.Portfolio]
    fromInternal = map (ApiTypes.toPortfolio . DB.entityVal)

-- |
--
historiesHandler :: MySQL.ConnectionPool -> String -> Servant.Handler [ApiTypes.Ohlcv]
historiesHandler pool codeStr =
    case Model.toTickerSymbol codeStr of
    Just ts -> M.liftIO $ takePrices pool ts
    Nothing -> Servant.throwError Servant.err400 { Servant.errBody = "error" }

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
proxyWebApi :: Proxy WebApi'
proxyWebApi = Proxy

--
--
app :: Config -> Servant.Application
app c =
    Servant.serve proxyWebApi $ webApiServer c

-- |
-- Web API サーバーを起動する
runWebServer :: Conf.Info -> ApiTypes.ServerTChan -> IO ()
runWebServer conf chan = do
    pool <- ML.runNoLoggingT . MR.runResourceT $ MySQL.createMySQLPool connInfo poolSize
    Warp.runSettings settings . app $ Config conf pool chan
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize = 8
    settings = Warp.setPort 8739 Warp.defaultSettings

