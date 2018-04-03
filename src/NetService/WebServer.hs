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
Module      :  WebAPI.Server
Description :  REST Web API server
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

REST API サーバーモジュールです
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeOperators     #-}
module WebAPI.Server
    ( runWebAPI
    ) where
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Aeson                   as Aeson
import           Data.Int                     (Int64)
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import qualified Data.Time                    as Tm
import           Database.Persist             ((==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import           GHC.Generics
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Servant                      as S
import           Servant.API

import           BackOffice.Agency            (updateSomeRates)
import           Conf
import           Lib                          (toISO8601DateTime, tzAsiaTokyo)
import           Model

data Config = Config
    { cConf :: Conf.Info
    , cPool :: MySQL.ConnectionPool
    }

--
--
data ReplyPortfolio = ReplyPortfolio
    { code     :: String
    , caption  :: Maybe String
    , updateAt :: Maybe String
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON ReplyPortfolio
instance Aeson.ToJSON ReplyPortfolio

--
--
toReplyPortfolio :: Portfolio -> ReplyPortfolio
toReplyPortfolio o = ReplyPortfolio
    { code     = fromTickerSymbol $ portfolioTicker o
    , caption  = T.unpack <$> portfolioCaption o
    , updateAt = toISO8601DateTime
                . Tm.utcToZonedTime tzAsiaTokyo
                <$> portfolioUpdateAt o
    }

--
--
data ReplyOhlcv = ReplyOhlcv
    { at     :: String
    , open   :: Maybe Double
    , high   :: Maybe Double
    , low    :: Maybe Double
    , close  :: Maybe Double
    , volume :: Int64
    , source :: Maybe String
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON ReplyOhlcv
instance Aeson.ToJSON ReplyOhlcv

--
--
toReplyOhlcv :: Ohlcv -> ReplyOhlcv
toReplyOhlcv o = ReplyOhlcv
    { at     = toISO8601DateTime
                . Tm.utcToZonedTime tzAsiaTokyo
                $ ohlcvAt o
    , open   = ohlcvOpen o
    , high   = ohlcvHigh o
    , low    = ohlcvLow o
    , close  = ohlcvClose o
    , volume = ohlcvVolume o
    , source = T.unpack <$> ohlcvSource o
    }

--
--
type WebAPI
    =   "update" :> "rates" :> Get '[JSON] String
    :<|>"version" :> Get '[JSON] MyPackageYaml
    :<|>"portfolio" :> Get '[JSON] [ReplyPortfolio]
    :<|>"stocks" :> "history" :> QueryParam "code" String :> Get '[JSON] [ReplyOhlcv]

webAPI :: Proxy WebAPI
webAPI = Proxy

--
--
webApiServer :: Config -> S.Server WebAPI
webApiServer cnf =
    update :<|> version :<|> portfolio :<|> stocksHistory
    where
    update = updateRatesHandler cnf
    version = versionHandler
    portfolio = portfolioHandler (cPool cnf)
    stocksHistory = stocksHistoryHandler (cPool cnf)


-- |
--
updateRatesHandler :: Config -> S.Handler String
updateRatesHandler cnf = do
    M.liftIO . updateSomeRates $ cConf cnf
    return "perform update rates."

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
stocksHistoryHandler :: MySQL.ConnectionPool -> Maybe String -> S.Handler [ReplyOhlcv]
stocksHistoryHandler pool codeStr =
    case toTickerSymbol =<< codeStr of
        Just ts -> do
            M.liftIO $ print ts
            M.liftIO $ prices ts
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

--
--
app :: Config -> S.Application
app = S.serve webAPI . webApiServer

-- |
-- Web API サーバーを起動する
runWebAPI :: Conf.Info -> IO ()
runWebAPI conf = do
    pool <- ML.runNoLoggingT . MR.runResourceT $ MySQL.createMySQLPool connInfo poolSize
    Warp.runSettings settings . app $ Config conf pool
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    poolSize = 8
    settings = Warp.setPort 8740 Warp.defaultSettings

