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
Module      :  NetService.RestApiHandler
Description :  REST API handler
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

REST API モジュールです
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module NetService.RestApiHandler
    ( updateHistoriesHandler
    , publishZmqHandler
    , getPortfolioHandler
    , getChartHandler
    --
    -- stocks histories CRUD
    --
    , getHistoriesHandler
    , putHistoriesHandler
    , patchHistoriesHandler
    , deleteHistoriesHandler
    )
where
import qualified Control.Concurrent.STM        as STM
import qualified Control.Monad                 as M
import qualified Control.Monad.IO.Class        as M
import qualified Control.Monad.Trans           as MonadTrans
import           Control.Monad.Trans.Either               ( EitherT
                                                          , newEitherT
                                                          , runEitherT
                                                          )
import           Control.Monad.Trans.Maybe                ( MaybeT(..) )
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.CaseInsensitive          as CI
import           Data.Default                             ( Default(..) )
import qualified Data.Either                   as Either
import qualified Data.Maybe                    as Maybe
import           Database.Persist                         ( (=.)
                                                          , (==.)
                                                          )
import qualified Database.Persist              as DB
import qualified Database.Persist.MySQL        as MySQL
import qualified Database.Persist.Sql          as DB
import qualified Servant
import           System.IO.Temp                           ( withSystemTempDirectory
                                                          )

import qualified BackOffice.Agency             as Agency
import qualified Conf
import qualified Model
import           NetService.ApiTypes                      ( ApiOhlcv
                                                          , ApiPortfolio
                                                          , Chart(..)
                                                          , ChartWithCacheControl
                                                          , MarketCode
                                                          , RecordsLimit(..)
                                                          , ServerTChan
                                                          , SvgBinary(..)
                                                          , defQueryHeight
                                                          , defQueryWidth
                                                          , fromApiOhlcv
                                                          , toApiOhlcv
                                                          , toApiPortfolio
                                                          )
import           NetService.HttpError
import qualified NetService.PlotChart          as PlotChart

-- |
--
updateHistoriesHandler :: Conf.Info -> Servant.Handler Servant.NoContent
updateHistoriesHandler cnf = do
    M.liftIO $ Agency.updateSomeRates cnf
    return Servant.NoContent

-- |
--
publishZmqHandler
    :: ServerTChan
    -> MySQL.ConnectionPool
    -> MarketCode
    -> Servant.Handler Servant.NoContent
publishZmqHandler chan pool codeStr = case Model.toTickerSymbol codeStr of
    Nothing -> err400BadRequest . BL8.pack $ unwords
        ["market code", codeStr, "is unknown"]
    Just ts -> do
        M.liftIO $ publish ts
        return Servant.NoContent
  where
    --
    --
    publish :: Model.TickerSymbol -> IO ()
    publish = STM.atomically . STM.writeTChan chan M.<=< selectList
    --
    --
    selectList ticker = do
        xs <- DB.runSqlPersistMPool
            (DB.selectList [Model.OhlcvTicker ==. ticker] [DB.Asc Model.OhlcvAt]
            )
            pool
        pure [ toApiOhlcv $ DB.entityVal x | x <- xs ]

-- |
--
getPortfolioHandler :: MySQL.ConnectionPool -> Servant.Handler [ApiPortfolio]
getPortfolioHandler pool = M.liftIO selectList
  where
    selectList = do
        xs <- DB.runSqlPersistMPool
            (DB.selectList [] [DB.Asc Model.PortfolioTicker])
            pool
        pure [ toApiPortfolio $ DB.entityVal x | x <- xs ]

-- |
--
getChartHandler
    :: MySQL.ConnectionPool
    -> MarketCode
    -> Model.TimeFrame
    -> Maybe Int
    -> Maybe Int
    -> Servant.Handler ChartWithCacheControl
getChartHandler pool codeStr timeFrame qWidth qHeight =
    case Model.toTickerSymbol codeBody of
        Nothing -> err400BadRequest . BL8.pack $ unwords
            ["market code", codeStr, "is unknown"]
        Just ticker | CI.mk codeSuffix == ".svg" -> go ticker
                    | otherwise                  -> err404NotFound
  where
    --
    --
    (codeBody, codeSuffix) = span (/= '.') codeStr
    --
    --
    chartSize =
        ( Maybe.fromMaybe defQueryWidth qWidth
        , Maybe.fromMaybe defQueryHeight qHeight
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
                { cSize   = chartSize
                , cTitle  = "stock prices"
                , cOhlcvs = ohlcvs
                }
        Servant.addHeader "private, no-store, no-cache, must-revalidate"
            .   Chart
            <$> chartSVG chartData
    --
    --
    selectList ticker = flip DB.runSqlPersistMPool pool $ DB.selectList
        [Model.OhlcvTf ==. timeFrame, Model.OhlcvTicker ==. ticker]
        [DB.Desc Model.OhlcvAt, DB.LimitTo 100]

-- |
-- 取得(SELECT)
getHistoriesHandler
    :: MySQL.ConnectionPool
    -> MarketCode
    -> Model.TimeFrame
    -> Maybe RecordsLimit
    -> Servant.Handler [ApiOhlcv]
getHistoriesHandler pool codeStr timeFrame maybeLimit =
    case Model.toTickerSymbol codeStr of
        Nothing -> err400BadRequest . BL8.pack $ unwords
            ["market code", codeStr, "is unknown"]
        Just ticker ->
            map (toApiOhlcv . DB.entityVal) <$> M.liftIO (selectList ticker)
  where
    --
    --
    limit = Maybe.fromMaybe (def :: RecordsLimit) maybeLimit
    --
    --
    selectList ticker = DB.runSqlPersistMPool
        (DB.selectList
            [Model.OhlcvTf ==. timeFrame, Model.OhlcvTicker ==. ticker]
            [DB.Desc Model.OhlcvAt, DB.LimitTo (unRecordsLimit limit)]
        )
        pool

-- |
-- 新規挿入(INSERT)
putHistoriesHandler
    :: MySQL.ConnectionPool
    -> MarketCode
    -> Model.TimeFrame
    -> [ApiOhlcv]
    -> Servant.Handler [ApiOhlcv]
putHistoriesHandler pool codeStr timeFrame apiOhlcvs =
    case Model.toTickerSymbol codeStr of
        Nothing -> err400BadRequest . BL8.pack $ unwords
            ["market code", codeStr, "is unknown"]
        Just ticker -> do
            responce <- M.mapM (M.liftIO . runEitherT . insert ticker) apiOhlcvs
            case (Either.lefts responce, Either.rights responce) of
                ([], rights) ->
                    -- 成功
                    return rights
                (lefts, _) ->
                    -- 部分的成功または失敗
                    err409Conflict (Aeson.encode lefts)
  where
    --
    --
    insert :: Model.TickerSymbol -> ApiOhlcv -> EitherT ApiOhlcv IO ApiOhlcv
    insert ticker apiOhlcv =
        let err _ = newEitherT . return $ Left apiOhlcv
            ok _ = newEitherT . return $ Right apiOhlcv
        in  case fromApiOhlcv ticker timeFrame apiOhlcv of
                Left  x     -> err x
                Right ohlcv -> do
                    responce <- MonadTrans.lift $ insertOhlcv pool ohlcv
                    either err ok responce

-- |
-- データを入れる(INSERT)
insertOhlcv :: MySQL.ConnectionPool -> Model.Ohlcv -> IO (Either () ())
insertOhlcv pool ohlcv@Model.Ohlcv {..} = DB.runSqlPersistMPool go pool
  where
    go = do
        -- 同じ時間の物があるか探してみる
        entity <- DB.selectFirst
            [ Model.OhlcvTicker ==. ohlcvTicker
            , Model.OhlcvTf ==. ohlcvTf
            , Model.OhlcvAt ==. ohlcvAt
            ]
            []
        -- 同じ時間の物が有った場合は何もしない, 無かった場合は新規挿入
        case entity of
            Just _  -> return (Left ())
            Nothing -> DB.insert ohlcv >> return (Right ())

-- |
-- 既存のデータを入れ替える(UPDATE / INSERT)
patchHistoriesHandler
    :: MySQL.ConnectionPool
    -> MarketCode
    -> Model.TimeFrame
    -> [ApiOhlcv]
    -> Servant.Handler [ApiOhlcv]
patchHistoriesHandler pool codeStr timeFrame apiOhlcvs =
    case Model.toTickerSymbol codeStr of
        Nothing -> err400BadRequest . BL8.pack $ unwords
            ["market code", codeStr, "is unknown"]
        Just ticker ->
            Maybe.catMaybes
                <$> M.mapM (M.liftIO . runMaybeT . store ticker) apiOhlcvs
  where
    --
    --
    store :: Model.TickerSymbol -> ApiOhlcv -> MaybeT IO ApiOhlcv
    store ticker apiOhlcv = case fromApiOhlcv ticker timeFrame apiOhlcv of
        Left  _   -> MaybeT (return Nothing)
        Right val -> do
            MonadTrans.lift (insertOrUpdateOhlcv pool val)
            MaybeT (return $ Just apiOhlcv)

-- |
-- データを入れる
-- もしくは
-- 既存のデータを入れ替える(UPDATE / INSERT)
insertOrUpdateOhlcv :: MySQL.ConnectionPool -> Model.Ohlcv -> IO ()
insertOrUpdateOhlcv pool ohlcv@Model.Ohlcv {..} = DB.runSqlPersistMPool go pool
  where
    go = do
        -- 同じ時間の物があるか探してみる
        entity <- DB.selectFirst
            [ Model.OhlcvTicker ==. ohlcvTicker
            , Model.OhlcvTf ==. ohlcvTf
            , Model.OhlcvAt ==. ohlcvAt
            ]
            []
        case DB.entityKey <$> entity of
            -- レコードが無かった場合は新規挿入する
            Nothing  -> M.void $ DB.insert ohlcv
            -- レコードが有った場合は更新する
            Just key -> DB.update
                key
                [ Model.OhlcvOpen =. ohlcvOpen
                , Model.OhlcvHigh =. ohlcvHigh
                , Model.OhlcvLow =. ohlcvLow
                , Model.OhlcvClose =. ohlcvClose
                , Model.OhlcvVolume =. ohlcvVolume
                , Model.OhlcvSource =. ohlcvSource
                ]

-- |
-- 既存のデータを削除する(DELETE)
deleteHistoriesHandler
    :: MySQL.ConnectionPool
    -> MarketCode
    -> Model.TimeFrame
    -> [ApiOhlcv]
    -> Servant.Handler [ApiOhlcv]
deleteHistoriesHandler pool codeStr timeFrame apiOhlcvs =
    case Model.toTickerSymbol codeStr of
        Nothing -> err400BadRequest . BL8.pack $ unwords
            ["market code", codeStr, "is unknown"]
        Just ticker ->
            Maybe.catMaybes
                <$> M.mapM (M.liftIO . runMaybeT . delete ticker) apiOhlcvs
  where
    --
    --
    delete :: Model.TickerSymbol -> ApiOhlcv -> MaybeT IO ApiOhlcv
    delete ticker apiOhlcv = case fromApiOhlcv ticker timeFrame apiOhlcv of
        Left  _     -> MaybeT (return Nothing)
        Right ohlcv -> do
            let err _ = MaybeT $ return Nothing
            let ok _ = MaybeT $ return (Just apiOhlcv)
            result <- MonadTrans.lift $ deleteOhlcv pool ohlcv
            either err ok result

-- |
-- データを削除する(DELETE)
deleteOhlcv :: MySQL.ConnectionPool -> Model.Ohlcv -> IO (Either () ())
deleteOhlcv pool Model.Ohlcv {..} = DB.runSqlPersistMPool go pool
  where
    go = do
        -- 同じ物があるか探してみる
        entity <- DB.selectFirst
            [ Model.OhlcvTicker ==. ohlcvTicker
            , Model.OhlcvTf ==. ohlcvTf
            , Model.OhlcvAt ==. ohlcvAt
            , Model.OhlcvOpen ==. ohlcvOpen
            , Model.OhlcvHigh ==. ohlcvHigh
            , Model.OhlcvLow ==. ohlcvLow
            , Model.OhlcvClose ==. ohlcvClose
            , Model.OhlcvVolume ==. ohlcvVolume
            , Model.OhlcvSource ==. ohlcvSource
            ]
            []
        case DB.entityKey <$> entity of
            -- 無かった場合は何もしない
            Nothing  -> return $ Left ()
            -- 有った場合は削除する
            Just key -> do
                DB.deleteWhere [Model.OhlcvId ==. key]
                return $ Right ()

