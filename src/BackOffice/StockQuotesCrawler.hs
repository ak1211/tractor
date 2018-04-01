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
Module      :  BackOffice.StockQuotesCrawler
Description :
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

WEBスクレイピングしてきた株式情報を
MariaDBデータベースに入れる
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module BackOffice.StockQuotesCrawler
    ( runWebCrawlingPortfolios
    ) where
import           Control.Applicative          ((<|>))
import qualified Control.Concurrent           as CC
import           Control.Exception.Safe
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Conduit                 as C
import qualified Data.List                    as List
import qualified Data.Maybe                   as Mb
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import qualified Data.Text.Lazy.Builder.Int   as TLB
import qualified Data.Time                    as Tm
import           Database.Persist             ((<=.), (=.), (==.), (||.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified System.Random.MWC            as Random

import qualified BrokerBackend                as BB
import qualified Conf
import qualified GenBroker                    as GB
import qualified KabuCom.Scraper              as S
import           Lib                          (NthOfTotal, packNthOfTotal,
                                               tzAsiaTokyo)
import           Model
import           Scheduling                   (AsiaTokyoDay (..))
import qualified SinkSlack                    as Slack

-- |
-- インターネット上の株価情報を取りに行く関数
fetchStockPrices    :: (M.MonadIO m, MonadThrow m)
                    => BB.HTTPSession
                    -> TickerSymbol
                    -> m (Maybe T.Text, [Ohlcv])
fetchStockPrices sess ticker = do
    href <- accessURI ticker
    response <- M.liftIO $ BB.fetchHTTPInRelativePath sess href
    page <- S.stockDetailPage . BB.takeBodyFromResponse $ response
    let capt = S.sdpCaption page
        ohlcv = map (pack href) $ S.sdpDailyHistories page
    pure (Just capt, ohlcv)
    where
    --
    --
    pack :: String -> S.DailyStockPrice -> Ohlcv
    pack _ pr =
        let closingTime = Tm.TimeOfDay 15 00 00
            lt = Tm.LocalTime
                    { Tm.localDay = getAsiaTokyoDay $ S.dspDay pr
                    , Tm.localTimeOfDay = closingTime
                    }
        in
        Ohlcv
            { ohlcvTicker = ticker
            , ohlcvTf = TF1d
            , ohlcvAt = Tm.localTimeToUTC tzAsiaTokyo lt
            , ohlcvOpen = S.dspOpen pr
            , ohlcvHigh = S.dspHigh pr
            , ohlcvLow = S.dspLow pr
            , ohlcvClose = S.dspClose pr
            , ohlcvVolume = S.dspVolume pr
            , ohlcvSource = Just "kabu.com"     -- kabu.comより得た株価なので
            }
    -- |
    -- アクセスURI
    accessURI :: MonadThrow m => TickerSymbol -> m String
    -- 東証:個別株
    accessURI (TSTYO c) =
        let base = "/Light/TradeTool/StockDetail.asp"
            code = "StockCode=" ++ show c
            tokyo = "Market=1"
            href = base ++ "?" ++ code ++ "&" ++ tokyo
        in
        pure href
    -- 日経平均株価
    accessURI TSNI225 = throwString "Nikkei 225 is not supported"
    -- TOPIX
    accessURI TSTOPIX = throwString "TOPIX is not supported"

-- |
-- ポートフォリオの株価情報を取りに行く関数
runWebCrawlingPortfolios :: Conf.Info -> IO ()
runWebCrawlingPortfolios conf =
    case take 1 [x | (Conf.KabuCom x)<-Conf.brokers conf] of
    [broker] ->
        takeUpdateItems connInfo
        >>= \case
        [] -> C.runConduit $ C.yield "今回の更新は不要です。" C..| slack
        xs ->
            -- kabu.comへログインする
            MR.runResourceT . GB.siteConn broker ua $ go xs
    _ -> return ()
    where
    --
    --
    slack =
        let c = Conf.slack conf
        in
        Slack.simpleTextMsg c C..| Slack.sink c
    --
    --
    go :: [DB.Entity Portfolio] -> BB.HTTPSession -> IO ()
    go items sess =
        let msg = "更新対象は全部で" <> TLB.decimal (length items)  <> "個有ります。"
            -- アクセス毎の停止アクションをリストに挟み込んで
            -- アクション毎に停止しながら更新する
            acts d = List.intersperse d . map (update sess) . packNthOfTotal $ items
        in do
        delay <- randomDelay <$> M.liftIO Random.createSystemRandom
        C.runConduit $ C.yield (TLB.toLazyText msg) C..| slack
        -- 更新アクション実行
        C.runConduit $ M.sequence_ (acts delay) C..| slack
        C.runConduit $ C.yield "以上で更新処理を終了します。" C..| slack
    --
    --
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf
    --
    --
    ua = Conf.userAgent conf
    --
    -- アクセス毎の停止アクション
    randomDelay rgen =
        M.liftIO $ CC.threadDelay =<< Random.uniformR bound rgen
        where
        -- 307msから561msまでの乱数
        bound = (lowerBound,upperBound)
        lowerBound = 307*1000
        upperBound = 561*1000
    --
    --
    update  :: M.MonadIO m
            => BB.HTTPSession
            -> (DB.Entity Portfolio, NthOfTotal)
            -> C.ConduitT () TL.Text m ()
    update sess (pf,nth) = do
        putDescription (DB.entityVal pf) nth    -- 更新処理対象銘柄を説明する
        updateTimeAndSales sess conf pf         -- 更新処理本体

--
--
updateTimeAndSales  :: M.MonadIO m
                    => BB.HTTPSession
                    -> Conf.Info
                    -> DB.Entity Portfolio
                    -> C.ConduitT () TL.Text m ()
updateTimeAndSales sess conf (DB.Entity wKey wVal) =
    M.liftIO . ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        -- インターネットから株価情報を取得する
        (caption, ohlcvs) <- M.liftIO $ fetchStockPrices sess (portfolioTicker wVal)
        updateAt <- M.liftIO Tm.getCurrentTime
        -- 6本値をohlcvテーブルに書き込む
        M.forM_ ohlcvs $ \val ->
            -- 同じ時間の物があるか探してみる
            DB.selectFirst  [ OhlcvTicker ==. ohlcvTicker val
                            , OhlcvTf ==. ohlcvTf val
                            , OhlcvAt ==. ohlcvAt val] []
            >>= \case
            -- レコードが無かった場合は新規挿入する
            Nothing -> M.void $ DB.insert val
            -- レコードが有った場合は6本値とソース元だけ更新する
            Just (DB.Entity updKey _) ->
                DB.update updKey
                    [ OhlcvOpen     =. ohlcvOpen val
                    , OhlcvHigh     =. ohlcvHigh val
                    , OhlcvLow      =. ohlcvLow val
                    , OhlcvClose    =. ohlcvClose val
                    , OhlcvVolume   =. ohlcvVolume val
                    , OhlcvSource   =. ohlcvSource val
                    ]
        -- 更新日等をPortfolioテーブルに
        DB.update wKey
            [ PortfolioCaption  =. (portfolioCaption wVal <|> caption)  -- 以前から有る銘柄名を優先的に選択する
            , PortfolioUpdateAt =. Just updateAt
            ]
    where
    connInfo = Conf.connInfoDB $ Conf.mariaDB conf

--
--
takeUpdateItems :: MySQL.ConnectInfo -> IO [DB.Entity Portfolio]
takeUpdateItems connInfo = do
    limitTm <- diffTime <$> Tm.getCurrentTime
    -- 前回の更新から一定時間以上経過した更新対象のリストを得る
    ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateQuotes
        DB.selectList ([PortfolioUpdateAt ==. Nothing] ||. [PortfolioUpdateAt <=. Just limitTm]) []
    where
    --
    -- (-12)時間の足し算は12時間の引き算になる
    diffTime :: Tm.UTCTime -> Tm.UTCTime
    diffTime = Tm.addUTCTime $ fromInteger (-12*60*60)


-- |
-- 更新銘柄の説明を送る関数
putDescription  :: M.MonadIO m
                => Portfolio
                -> NthOfTotal
                -> C.ConduitT () TL.Text m ()
putDescription pf (current,total) =
    C.yield $ TLB.toLazyText msg
    where
    defaultCaption = T.pack . showTickerSymbol $ portfolioTicker pf
    textCaption = Mb.fromMaybe defaultCaption $ portfolioCaption pf
    caption = TLB.fromText textCaption
    bracketL = TLB.singleton '['
    bracketR = TLB.singleton ']'
    nOfm n m = TLB.decimal n <> TLB.singleton '/' <> TLB.decimal m
    trailer = bracketL <> nOfm current total <> bracketR
    msg = caption <> "を更新中。" <> trailer

