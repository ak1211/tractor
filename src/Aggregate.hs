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
Module      :  Aggregate
Description :  
Copyright   :  (c) 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

データベース上の情報を集計する。
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
module Aggregate
    (runAggregateOfPortfolios
    ) where

import qualified Safe
import qualified Data.Maybe as Mb
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Monoid ((<>))

import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M

import qualified Control.Monad.Trans.Resource as MR
import Control.Monad.Logger
import Database.Persist ((==.), (>=.))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Esqueleto as E
import qualified Data.Time.Calendar as Tm
import qualified Data.Time.Clock as Tm
import qualified Data.Time.LocalTime as Tm
import Data.Conduit (($$), ($=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import TickerSymbol as Import
import TimeFrame as Import
import TechnicalIndicators (TechnicalInds(..))
import qualified TechnicalIndicators as TI
import Model
import Conf
import Lib

-- | 指定の指標を計算する
calcIndicator :: TechnicalInds -> [DB.Entity Ohlcvt] -> [(DB.Entity Ohlcvt, TechInds)]
calcIndicator indicator entities =
    case indicator of
    TISMA period                -> ohlcvtClose `apply` (TI.sma period)
    TIEMA period                -> ohlcvtClose `apply` (TI.ema period)
    TIRSI period                -> ohlcvtClose `apply` (TI.rsi period)
    TIMACD fastP slowP          -> ohlcvtClose `apply` (TI.macd fastP slowP)
    TIMACDSIG fastP slowP sigP  -> ohlcvtClose `apply` (TI.macdSignal fastP slowP sigP)
    TIPSYCHOLO period           -> ohlcvtClose `apply` (TI.psycologicalLine period)
    where
    apply :: (Ohlcvt -> Maybe Double) -> ([Double] -> [Maybe Double]) -> [(DB.Entity Ohlcvt, TechInds)]
    apply price formula =
        let entAndPrices = [(e,pr) | e<-entities, let (Just pr)=price $ DB.entityVal e] in
        let (es,ps) = unzip entAndPrices in
        let entAndValues = zip es $ formula ps in
        [(e, makeTI e v) | (e, Just v)<-entAndValues]
    --
    makeTI e v = TechInds
                    { techIndsOhlcvt    = DB.entityKey e
                    , techIndsInd       = indicator
                    , techIndsVal       = v
                    }

-- | UTCの日付時間のうち、日本時間の日付のみ比較
sameDayOfJST :: Ohlcvt -> Ohlcvt -> Bool
sameDayOfJST a b =
    let jstDay = Tm.localDay . Tm.utcToLocalTime jst . ohlcvtAt in
    jstDay a == jstDay b

-- | 始値, 高値, 安値, 終値, 出来高, 売買代金を集計する関数
aggregateOfOHLCVT   :: (Ohlcvt -> Maybe Ohlcvt) -- ^ 値の判定関数
                    -> [Ohlcvt]                 -- ^ 時系列通りで与えること
                    -> Maybe Ohlcvt
aggregateOfOHLCVT _ [] = Nothing                -- 空リストの結果は未定義
aggregateOfOHLCVT decide serials@(first:_) =
    decide -- 集計結果が有効か無効かの判定をゆだねる。
        Ohlcvt
            { ohlcvtTicker      = ohlcvtTicker first
            , ohlcvtTf          = ohlcvtTf first
            , ohlcvtAt          = ohlcvtAt first
            -- 6本値は欠損値を許容する
            , ohlcvtOpen        = open  $ Mb.mapMaybe ohlcvtOpen serials
            , ohlcvtHigh        = high  $ Mb.mapMaybe ohlcvtHigh serials
            , ohlcvtLow         = low   $ Mb.mapMaybe ohlcvtLow serials
            , ohlcvtClose       = close $ Mb.mapMaybe ohlcvtClose serials
            , ohlcvtVolume      = vtSum $ Mb.mapMaybe ohlcvtVolume serials
            , ohlcvtTurnover    = vtSum $ Mb.mapMaybe ohlcvtTurnover serials
            --
            , ohlcvtSource      = T.append "agg from " <$> ohlcvtSource first
            }
    where
    -- | 集計期間中の初値
    open :: [Double] -> Maybe Double
    open  = Safe.headMay
    -- | 集計期間中の最高値
    high :: [Double] -> Maybe Double
    high  = Safe.maximumMay
    -- | 集計期間中の最安値
    low :: [Double] -> Maybe Double
    low   = Safe.minimumMay
    -- | 集計期間中の終値
    close :: [Double] -> Maybe Double
    close = Safe.headMay . reverse
    -- | 集計期間中の総和
    vtSum :: [Double] -> Maybe Double
    vtSum [] = Nothing
    vtSum xs = Just $ sum xs

-- | 1時間足を日足にする集計処理
aggregate :: MySQL.ConnectInfo -> TickerSymbol -> IO ()
aggregate connInfo tickerSymbol =
    runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        -- 最新の日足を取り出す
        latestDaily <- DB.selectFirst
                        [ OhlcvtTicker  ==. tickerSymbol
                        , OhlcvtTf      ==. TF1d ]
                        [ DB.Desc OhlcvtAt, DB.LimitTo 1 ]
        -- 最新の日足の次の日以降を取り出すフィルター
        let filtTomorrow =  [ OhlcvtAt  >=. nday
                            | e<-Mb.maybeToList latestDaily
                            , let nday = nextDay . ohlcvtAt $ DB.entityVal e
                            ]
        -- 1時間足の歩み値を取り出すフィルター
        let filtHourly =    [ OhlcvtTicker  ==. tickerSymbol
                            , OhlcvtTf      ==. TF1h    -- 1時間足
                            ] ++ filtTomorrow
        -- 1時間足を日足に集計する処理
        timeAndSales <- DB.selectSource (filtHourly :: [DB.Filter Ohlcvt]) [DB.Asc OhlcvtAt]
                        $= CL.map DB.entityVal
                        $= CL.groupBy sameDayOfJST
                        $= CL.map (aggregateOfOHLCVT decisionOfDailyPrices)
                        $= CL.catMaybes
                        $$ CL.consume
        -- 集計後の日足をデーターベースへ
        -- 最新の日足以降を対象にしている原理上、
        -- 同じ物はないので無条件で新規挿入する
        M.mapM_ DB.insert timeAndSales
    where
    -- | 次の日
    nextDay :: Tm.UTCTime -> Tm.UTCTime
    nextDay (Tm.UTCTime day time) = Tm.UTCTime (Tm.addDays 1 day) time
    -- | 集計後の値は(00:00:00 JST)をUTCに変換した日付(09:00:00 UTC)にする
    decisionOfDailyPrices :: Ohlcvt -> Maybe Ohlcvt
    decisionOfDailyPrices val =
        let timeOfJST = Tm.utcToLocalTime jst $ ohlcvtAt val in
        let timeOfUTC = Tm.localTimeToUTC jst $ timeOfJST {Tm.localTimeOfDay = Tm.midnight} in
        Just $ val {ohlcvtTf = TF1d, ohlcvtAt = timeOfUTC}

-- | インディケータの計算
calculate   :: MySQL.ConnectInfo
            -> TickerSymbol
            -> TimeFrame
            -> TechnicalInds
            -> IO ()
calculate connInfo tickerSymbol timeFrame indicator =
    runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        -- どこまで計算を進めたかをデーターベースに問い合わせる
        latests <- E.select $
                    E.from $ \(ohlcvt `E.InnerJoin` ti) -> do
                        E.on
                                (ohlcvt E.^. OhlcvtId E.==. ti E.^. TechIndsOhlcvt)
                        E.where_ $
                                ohlcvt E.^. OhlcvtTicker E.==. E.val tickerSymbol
                                E.&&.
                                ohlcvt E.^. OhlcvtTf E.==. E.val timeFrame
                                E.&&.
                                ti E.^. TechIndsInd E.==. E.val indicator
                        E.orderBy
                                [E.desc (ohlcvt E.^. OhlcvtAt)]
                        E.limit 1
                        return (ohlcvt E.^. OhlcvtAt)
        let latestAt = Mb.listToMaybe [x | (E.Value x) <- latests]
        -- 計算に必要な値を取り出す
        let filt =  [ OhlcvtTicker  ==. tickerSymbol
                    , OhlcvtTf      ==. timeFrame
                    ]
        source <- DB.selectList filt [DB.Asc OhlcvtAt]
        -- インディケータの計算
        let values = calcIndicator indicator source
        -- すでにある物はデーターベースに入れない
        let insertionValues = filter (not . hasAlreadyEntity latestAt . fst) values
        M.mapM_ (DB.insert . snd) insertionValues
    where
    -- | データーベースにすでにあるか？
    hasAlreadyEntity :: Maybe Tm.UTCTime -> DB.Entity Ohlcvt -> Bool
    hasAlreadyEntity Nothing _         = False
    hasAlreadyEntity (Just latest) ent =
        let this = ohlcvtAt $ DB.entityVal ent in
        (latest >= this)

-- | Portfolio上の情報を集計する関数
runAggregateOfPortfolios :: M.MonadIO m => Conf.Info -> C.Source m TL.Text
runAggregateOfPortfolios conf = do
    -- 処理対象銘柄の数
    counts <- M.liftIO . runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        DB.count ([] :: [DB.Filter Portfolio])

    C.yield . TB.toLazyText $
        case counts of
        0 -> "集計処理の対象銘柄がありません。"
        x -> "集計処理の対象銘柄は全部で" <> TB.decimal x <> "個有ります。"

    -- 処理対象銘柄のリストを得る
    ps  <- M.liftIO . runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        map DB.entityVal <$> DB.selectList [] [DB.Asc PortfolioId]

    -- 銘柄毎に集計処理
    M.forM_ (zip ps [1..]) $ \(val, number) -> do
        let ticker = portfolioTicker val
        let textCaption = Mb.fromMaybe (T.pack . show $ portfolioTicker val) $ portfolioCaption val
        let progress=  TB.singleton '[' <> TB.decimal (number :: Int)
                    <> TB.singleton '/' <> TB.decimal counts
                    <> TB.singleton ']'
        C.yield . TB.toLazyText $ TB.fromText textCaption <> " を集計中。" <> progress
        M.liftIO $ do
            aggregate connInfo ticker
            --
            let calc = calculate connInfo ticker
            --
            calc TF1d (TISMA 5)
            calc TF1d (TISMA 10)
            calc TF1d (TISMA 25)
            calc TF1d (TISMA 75)
            --
            calc TF1d (TIEMA 5)
            calc TF1d (TIEMA 10)
            calc TF1d (TIEMA 25)
            calc TF1d (TIEMA 75)
            --
            calc TF1d (TIRSI 9)
            calc TF1d (TIRSI 14)
            --
            calc TF1d (TIMACD 12 26)
            calc TF1d (TIMACDSIG 12 26 9)
            --
            calc TF1d (TIPSYCHOLO 12)

    C.yield "集計処理を終了します。"
    where
    --
    connInfo :: MySQL.ConnectInfo
    connInfo =
        let mdb = Conf.mariaDB conf in
        MySQL.defaultConnectInfo
            { MySQL.connectHost = Conf.host mdb
            , MySQL.connectPort = Conf.port mdb
            , MySQL.connectUser = Conf.user mdb
            , MySQL.connectPassword = Conf.password mdb
            , MySQL.connectDatabase = Conf.database mdb
            }

