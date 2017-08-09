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

import qualified Control.Arrow as A
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
import Data.Typeable (Typeable)

import TickerSymbol as Import
import TimeFrame as Import
import TechnicalIndicators (TechnicalInds(..))
import qualified TechnicalIndicators as TI
import Model
import Conf
import Lib

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
        timeAndSales <- DB.selectSource (filtHourly :: [DB.Filter Ohlcvt]) [DB.Desc OhlcvtAt]
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
    -- | 集計後の値は(00:00:00 JST)をUTCに変換した日付(15:00:00 UTC)にする
    decisionOfDailyPrices :: Ohlcvt -> Maybe Ohlcvt
    decisionOfDailyPrices val =
        let timeOfJST = Tm.utcToLocalTime jst $ ohlcvtAt val in
        let timeOfUTC = Tm.localTimeToUTC jst $ timeOfJST {Tm.localTimeOfDay = Tm.midnight} in
        Just $ val {ohlcvtTf = TF1d, ohlcvtAt = timeOfUTC}

-- データーベースにテクニカル指標を問い合わせる
queryIndicatorsDesc :: MySQL.ConnectInfo
                -> TickerSymbol
                -> TimeFrame
                -> TechnicalInds
                -> IO [(DB.Entity Ohlcvt, Double)]
queryIndicatorsDesc connInfo tickerSymbol timeFrame indicator =
    runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        E.select $
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
                return
                    ( ohlcvt
                    , ti        E.^. TechIndsVal
                    )
    >>= return . map (A.second E.unValue)

-- | インディケータの計算
calculate   :: MySQL.ConnectInfo
            -> TickerSymbol
            -> TimeFrame
            -> TechnicalInds
            -> IO ()
--
calculate connInfo tickerSymbol timeFrame indicator =
    case indicator of
    TIMACD fastP slowP        -> do
        -- MACDは短期,長期EMAを先に計算してからテクニカル指標テーブルより計算を行なう
        -- MACDの計算に使う短期,長期EMAの計算をする
        [fast,slow] <- M.mapM
                        (\x ->  calculate connInfo tickerSymbol timeFrame x
                                >> queryIndicatorsDesc connInfo tickerSymbol timeFrame x
                        )
                        [TIEMA fastP, TIEMA slowP]
        -- テクニカル指標テーブルの短期,長期EMAよりMACDの計算
        let macd = zipWith
                    (\(a,f) (b,s) ->
                        if DB.entityKey a /= DB.entityKey b then
                            error "calculate MACD error"
                        else
                            (a, TI.macdFormula f s)
                    ) fast slow
        -- データーベースに入れる
        prevItem <- queryPrevItem
        let prevAt  = ohlcvtAt . DB.entityVal . fst <$> prevItem
        runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
            DB.runMigration migrateQuotes
            M.mapM_ (DB.insert . uncurry packTI) $ takeWhile (isNewEntry prevAt . fst) macd
    --
    TIMACDSIG fastP slowP sigP  -> do
        -- MACDSIGはMACDを先に計算してからテクニカル指標テーブルより計算を行なう
        -- MACDの計算をする
        entAndMacd <-   (\x ->  calculate connInfo tickerSymbol timeFrame x
                                >> queryIndicatorsDesc connInfo tickerSymbol timeFrame x
                        ) (TIMACD fastP slowP)
        -- テクニカル指標テーブルのMACDSIGを取り出す
        prevItem <- queryPrevItem
        let prevAt  = ohlcvtAt . DB.entityVal . fst <$> prevItem
        -- テクニカル指標テーブルのMACDよりMACDSIGの計算
        let formula = case (snd <$> prevItem) of
                        Nothing -> TI.ema1 sigP
                        Just sd -> TI.ema sigP sd
        let macdSig = uncurry zip . A.second formula . unzip $ entAndMacd
        -- データーベースに入れる
        runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
            DB.runMigration migrateQuotes
            M.mapM_ (DB.insert . uncurry packTI) $ takeWhile (isNewEntry prevAt . fst) macdSig
    --
    _ ->
        -- それ以外の指標は株価テーブルより計算を行なう
        calculateOfIndicatorsFromOhlcvtTable
    where
    --
    calculateOfIndicatorsFromOhlcvtTable = do
        prevItem <- queryPrevItem
        let prevAt  = ohlcvtAt . DB.entityVal . fst <$> prevItem
        let prevVal = snd <$> prevItem
        runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
            DB.runMigration migrateQuotes
            -- 計算に必要な値を取り出す
            let filt =  [ OhlcvtTicker  ==. tickerSymbol
                        , OhlcvtTf      ==. timeFrame
                        ]
            source <- DB.selectList filt [DB.Desc OhlcvtAt]
            -- 指定の指標を計算する
            let values = takeWhile (isNewEntry prevAt . fst) $ calc indicator prevVal source
            -- データーベースに入れる
            M.mapM_ (DB.insert . snd) values
    -- データーベースに前回のEMAがあればそれを種にして計算を始める。
    calc (TIEMA period) prevVal
        | Mb.isNothing prevVal  = apply ohlcvtClose $ TI.ema1 period
        | otherwise             = apply ohlcvtClose $ TI.ema period (Mb.fromJust prevVal)
    --
    calc (TISMA period) _       = apply ohlcvtClose $ TI.sma period
    --
    calc (TIRSI period) _       = apply ohlcvtClose $ TI.rsi period
    -- ここのMACD,MACDSIGは最初から全計算する場合のもの
    -- 通常は使わない
    calc (TIMACD fa sl) _       = apply ohlcvtClose $ TI.macd fa sl
    calc (TIMACDSIG fa sl sg) _ = apply ohlcvtClose $ TI.macdSignal fa sl sg
    --
    calc (TIBBLOW3 period) _    = apply ohlcvtClose $ map (\(a,_,_,_,_,_,_) -> a) . TI.bollingerBands period
    calc (TIBBLOW2 period) _    = apply ohlcvtClose $ map (\(_,b,_,_,_,_,_) -> b) . TI.bollingerBands period
    calc (TIBBLOW1 period) _    = apply ohlcvtClose $ map (\(_,_,c,_,_,_,_) -> c) . TI.bollingerBands period
    calc (TIBBMIDDLE period) _  = apply ohlcvtClose $ map (\(_,_,_,d,_,_,_) -> d) . TI.bollingerBands period
    calc (TIBBUP1 period) _     = apply ohlcvtClose $ map (\(_,_,_,_,e,_,_) -> e) . TI.bollingerBands period
    calc (TIBBUP2 period) _     = apply ohlcvtClose $ map (\(_,_,_,_,_,f,_) -> f) . TI.bollingerBands period
    calc (TIBBUP3 period) _     = apply ohlcvtClose $ map (\(_,_,_,_,_,_,g) -> g) . TI.bollingerBands period
    --
    calc (TIPSYCHOLO period) _  = apply ohlcvtClose $ TI.psycologicalLine period
    --
    calc (TIDIPOS period) _     = apply priceHiLoClo $ (\(a,_,_) -> a) . unzip3 . TI.dmi period
    calc (TIDINEG period) _     = apply priceHiLoClo $ (\(_,b,_) -> b) . unzip3 . TI.dmi period
    calc (TIADX period) _       = apply priceHiLoClo $ (\(_,_,c) -> c) . unzip3 . TI.dmi period
    --
    priceHiLoClo :: Ohlcvt -> Maybe TI.PriceHiLoClo
    priceHiLoClo val = do
        hi <- ohlcvtHigh val
        lo <- ohlcvtLow val
        clo <- ohlcvtClose val
        Just (hi,lo,clo)
    --
    apply   :: (Typeable t)
            => (Ohlcvt -> Maybe t)
            -> ([t] -> [Double])
            -> [DB.Entity Ohlcvt]
            -> [(DB.Entity Ohlcvt, TechInds)]
    apply price formula entities =
        let ents = filter (Mb.isJust . price . DB.entityVal) entities in
        let values = formula $ map (Mb.fromJust . price . DB.entityVal) ents in
        zipWith (\e v -> (e, packTI e v)) ents values
    --
    packTI e v =
        TechInds    { techIndsOhlcvt    = DB.entityKey e
                    , techIndsInd       = indicator
                    , techIndsVal       = v
                    }
    -- | データーベースに入れるべきか？
    isNewEntry :: Maybe Tm.UTCTime -> DB.Entity Ohlcvt -> Bool
    isNewEntry Nothing _                    = True
    isNewEntry (Just timeOfPrevItem) entity =
        case ohlcvtAt $ DB.entityVal entity of
        curTime | timeOfPrevItem < curTime  -> True
                | otherwise                 -> False
    -- 前回までの実行でどこまで計算を進めたかをデーターベースに問い合わせる
    queryPrevItem =
        Mb.listToMaybe <$> queryIndicatorsDesc connInfo tickerSymbol timeFrame indicator


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
            let indicators =    [ TISMA 5
                                , TISMA 25
                                , TISMA 75
                                --
                                , TIEMA 5
                                , TIEMA 25
                                , TIEMA 75
                                --
                                , TIRSI 9
                                , TIRSI 14
                                --
                                , TIMACD 12 26
                                , TIMACDSIG 12 26 9
                                --
                                , TIBBLOW3 25
                                , TIBBLOW2 25
                                , TIBBLOW1 25
                                , TIBBMIDDLE 25
                                , TIBBUP1 25
                                , TIBBUP2 25
                                , TIBBUP3 25
                                --
                                , TIPSYCHOLO 12
                                --
                                , TIDIPOS 14
                                , TIDINEG 14
                                , TIADX 14
                                , TIADX 9
                                ]
            let timeFrame = [TF1h,TF1d]
            M.sequence_ [calculate connInfo ticker tf i | tf<-timeFrame, i<-indicators]

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

