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
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.WebServer
    ( module NetService.Types
    , runWebServer
    ) where
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (writeTChan)
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.Csv                     as Csv
import           Data.Proxy                   (Proxy (..))
import           Database.Persist             ((==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import           GHC.Conc                     (getNumCapabilities,
                                               getNumProcessors, numSparks)
import           GHC.Generics                 (Generic)
import           GHC.Stats                    (GCDetails (..), RTSStats (..),
                                               getRTSStats)
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Servant                      as S
import           Servant.API
import           System.Mem                   (performGC)

import           BackOffice.Agency            (updateSomeRates)
import           Conf
import           Model
import           NetService.Types

deriving instance Generic GCDetails
instance Aeson.FromJSON GCDetails
instance Aeson.ToJSON GCDetails

deriving instance Generic RTSStats
instance Aeson.FromJSON RTSStats
instance Aeson.ToJSON RTSStats

data Health = Health
    { hNumCapabilities :: Int
    , hNumProcessors   :: Int
    , hNumSparks       :: Int
    , hStats           :: RTSStats
    } deriving Generic
instance Aeson.FromJSON Health
instance Aeson.ToJSON Health

data Config = Config
    { cConf :: Conf.Info
    , cPool :: MySQL.ConnectionPool
    , cChan :: ServerTChan
    }

--
--
type WebAPI
    =   "update" :> "rates" :> Get '[JSON] NoContent
    :<|>"publish" :> QueryParam "code" String :> GetNoContent '[JSON] NoContent
    :<|>"health" :> Get '[JSON] Health
    :<|>"version" :> Get '[JSON] MyPackageYaml
    :<|>"portfolio" :> Get '[JSON] [ReplyPortfolio]
    :<|>"stocks" :> "history" :> "js" :> QueryParam "code" String :> Get '[JSON] [ReplyOhlcv]
    :<|>"stocks" :> "history" :> "csv" :> QueryParam "code" String :> Get '[PlainText] String

webAPI :: Proxy WebAPI
webAPI = Proxy

--
--
webApiServer :: Config -> S.Server WebAPI
webApiServer cnf =
    update
    :<|> publish
    :<|> health
    :<|> version
    :<|> portfolio
    :<|> stocksHistoryJS
    :<|> stocksHistoryCSV
    where
    update = updateRatesHandler cnf
    publish = publishHandler (cPool cnf) (cChan cnf)
    health = healthHandler
    version = versionHandler
    portfolio = portfolioHandler (cPool cnf)
    stocksHistoryJS = stocksHistoryJSHandler (cPool cnf)
    stocksHistoryCSV = stocksHistoryCSVHandler (cPool cnf)

-- |
--
updateRatesHandler :: Config -> S.Handler NoContent
updateRatesHandler cnf = do
    M.liftIO . updateSomeRates $ cConf cnf
    return NoContent

-- |
--
publishHandler :: MySQL.ConnectionPool -> ServerTChan -> Maybe String -> S.Handler NoContent
publishHandler pool chan codeStr =
    case toTickerSymbol =<< codeStr of
        Just ts -> do
            ps <- M.liftIO $ prices ts
            M.liftIO . atomically $ writeTChan chan ps
            return NoContent
        Nothing -> S.throwError S.err400 { S.errBody = "error" }
    where
    --
    --
    prices :: TickerSymbol -> IO [ReplyOhlcv]
    prices ticker =
        flip DB.runSqlPersistMPool pool $
            toReps <$> DB.selectList [OhlcvTicker ==. ticker] [DB.Asc OhlcvAt]
    --
    --
    toReps :: [DB.Entity Ohlcv] -> [ReplyOhlcv]
    toReps = map (toReplyOhlcv . DB.entityVal)

-- |
--
healthHandler :: S.Handler Health
healthHandler = do
    M.liftIO performGC
    sts <- M.liftIO getRTSStats
    c <- M.liftIO getNumCapabilities
    p <- M.liftIO getNumProcessors
    s <- M.liftIO numSparks
    return $ Health
        { hNumCapabilities = c
        , hNumProcessors = p
        , hNumSparks = s
        , hStats = sts
        }

-- |
--
versionHandler :: S.Handler MyPackageYaml
versionHandler =
    return myPackageYaml

-- |
--
portfolioHandler :: MySQL.ConnectionPool -> S.Handler [ReplyPortfolio]
portfolioHandler pool =
    M.liftIO pf
    where
    --
    --
    pf :: IO [ReplyPortfolio]
    pf =
        flip DB.runSqlPersistMPool pool $
            toReps <$> DB.selectList [] [DB.Asc PortfolioTicker]
    --
    --
    toReps :: [DB.Entity Portfolio] -> [ReplyPortfolio]
    toReps = map (toReplyPortfolio . DB.entityVal)

-- |
--
stocksHistoryJSHandler :: MySQL.ConnectionPool -> Maybe String -> S.Handler [ReplyOhlcv]
stocksHistoryJSHandler pool codeStr =
    case toTickerSymbol =<< codeStr of
    Just ts -> M.liftIO $ do
        print ts
        takePrices pool ts
    Nothing -> S.throwError S.err400 { S.errBody = "error" }

-- |
--
stocksHistoryCSVHandler :: MySQL.ConnectionPool -> Maybe String -> S.Handler String
stocksHistoryCSVHandler pool codeStr =
    case toTickerSymbol =<< codeStr of
    Just ts -> M.liftIO $ do
        print ts
        BL8.unpack . Csv.encodeByName replyOhlcvHeader <$> takePrices pool ts
    Nothing -> S.throwError S.err400 { S.errBody = "error" }

--
--
takePrices :: MySQL.ConnectionPool -> TickerSymbol -> IO [ReplyOhlcv]
takePrices pool ticker =
    flip DB.runSqlPersistMPool pool $
        toReps <$> DB.selectList [OhlcvTicker ==. ticker] [DB.Asc OhlcvAt]
    where
    --
    --
    toReps :: [DB.Entity Ohlcv] -> [ReplyOhlcv]
    toReps = map (toReplyOhlcv . DB.entityVal)

--
--
app :: Config -> S.Application
app = S.serve webAPI . webApiServer

-- |
-- Web API サーバーを起動する
runWebServer :: Conf.Info -> ServerTChan -> IO ()
runWebServer conf chan = do
    pool <- ML.runNoLoggingT . MR.runResourceT $ MySQL.createMySQLPool connInfo poolSize
    Warp.runSettings settings . app $ Config conf pool chan
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize = 8
    settings = Warp.setPort 8739 Warp.defaultSettings

