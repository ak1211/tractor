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
Module      :  Main
Description :  This file is main module of Application "Tractor"
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

アプリケーション「Tractor」のメインモジュールです。
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Arrow                as A
import qualified Control.Concurrent           as CC
import qualified Control.Concurrent.Async     as CC
import qualified Control.Exception            as Ex
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Trans.Resource as M
import           Data.Conduit                 (($$), ($=), (=$))
import qualified Data.Conduit                 as C
import           Data.Monoid                  ((<>))
import qualified Data.Semigroup               as S
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TB
import qualified Data.Text.Lazy.IO            as TL
import qualified Data.Time                    as Tm
import qualified Data.Time.Calendar.WeekDate  as Tm
import qualified Options.Applicative          as Opt

import qualified Aggregate
import qualified Conf
import qualified GenBroker                    as Broker
import qualified Lib
import qualified MatsuiCoJp.Broker
import qualified Scheduling
import qualified SinkSlack                    as Slack
import qualified StockQuotesCrawler           as Q

-- |
-- レポートをsinkSlackで送信する形式に変換する関数
reportMsg :: M.MonadIO m => Conf.Info -> C.Conduit Slack.Report m Slack.WebHook
reportMsg = Slack.reportMsg . Conf.slack

-- |
-- 組み立てられたメッセージをConf.Infoで指定されたSlackへ送る関数
sinkSlack :: Conf.Info -> C.Sink Slack.WebHook IO ()
sinkSlack = Slack.sink . Conf.slack

-- |
-- テキストメッセージをConf.InfoSlackで指定されたslackへ送る関数
toSlack :: Conf.InfoSlack -> TL.Text -> IO ()
toSlack conf msg =
    C.yield msg
    $= Slack.simpleTextMsg conf
    $$ Slack.sink conf

-- |
-- テキストメッセージをsinkSlackで送信する形式に変換する関数
simpleTextMsg :: M.MonadIO m => Conf.Info -> C.Conduit TL.Text m Slack.WebHook
simpleTextMsg = Slack.simpleTextMsg . Conf.slack

-- |
-- バッチ処理関数
batchProcessing :: Conf.Info -> IO ()
batchProcessing conf =
    webCrawling <> aggregate $$ sink
    where
    --
    --
    sink =
        Slack.simpleTextMsg (Conf.slack conf)
        =$ Slack.sink (Conf.slack conf)
    -- |
    -- Webクローリング
    webCrawling = Q.runWebCrawlingPortfolios conf
    -- |
    -- 集計
    aggregate = Aggregate.runAggregateOfPortfolios conf

-- |
-- バッチ処理スレッド
batchProcessThread :: Conf.Info -> Tm.Day -> IO ()
batchProcessThread conf jstDay =
    Scheduling.execute $ map Scheduling.packZonedTimeJob
        [(t, batchProcessing conf)
        | t<-Scheduling.batchProcessTimeInJST jstDay
        ]

-- |
-- 平日の報告スレッド
announceWeekdayThread :: Conf.Info -> Tm.Day -> IO ()
announceWeekdayThread conf jstDay =
    Scheduling.execute $ map Scheduling.packZonedTimeJob
        [(t, MatsuiCoJp.Broker.reportSecuritiesAnnounce conf)
        | t<-Scheduling.announceWeekdayTimeInJST jstDay
        ]

-- |
-- 休日の報告スレッド
announceHolidayThread :: Conf.Info -> Tm.Day -> IO ()
announceHolidayThread conf jstDay =
    Scheduling.execute $ map Scheduling.packZonedTimeJob
        [(t, announce)
        | t<-Scheduling.announceHolidayTimeInJST jstDay
        ]
    where
    --
    --
    msg =
        "本日 " <> TB.fromString (show jstDay) <> " は市場の休日です。"
    --
    --
    announce =
        C.yield (TB.toLazyText msg) $= simpleTextMsg conf $$ sinkSlack conf


-- |
-- 立会時間中のスレッド
tradingTimeThread :: Conf.Info -> [Tm.ZonedTime] -> IO ()
tradingTimeThread conf times =
    Scheduling.execute
        -- 3分前にログインする仕掛け
        $ map (timedelta (-180) . Scheduling.packZonedTimeJob)
        -- 再復帰は30分間隔
        [ (t, worker) | t<-Lib.every (30*60) times ]
    where
    --
    -- 実際の作業
    worker =
        M.runResourceT . MatsuiCoJp.Broker.siteConn conf $ \sess ->
            Scheduling.execute (fetchPriceJobs sess ++ reportJobs)
        `Ex.catches`
        --
        -- ここの例外ハンドラは例外再送出しないので、
        -- 例外処理の後は次のworkerに移る
        --
        -- 例外ハンドラ : 予想外のHTML
        [ Ex.Handler $ \(Broker.UnexpectedHTMLException ex) ->
            -- Slackへエラーメッセージを送る
            toSlack (Conf.slack conf) . TB.toLazyText
            $ "予想外のHTML例外 "
            <> TB.singleton '\"' <> TB.fromString (show ex) <> TB.singleton '\"'
        --
        -- 例外ハンドラ : 未保有株の売却指示
        , Ex.Handler $ \(Broker.DontHaveStocksToSellException ex) ->
            -- Slackへエラーメッセージを送る
            toSlack (Conf.slack conf) . TB.toLazyText
            $ "未保有株の売却指示 "
            <> TB.singleton '\"' <> TB.fromString (show ex) <> TB.singleton '\"'
        ]
    -- |
    -- 現在資産取得
    fetchPriceJobs session =
        map Scheduling.packZonedTimeJob
        [ (t, MatsuiCoJp.Broker.fetchPriceToStore conf session)    -- 現在資産取得関数
        | t<-Lib.every (Conf.recordAssetsInterval conf * 60) times
        ]
    -- |
    -- 現在資産評価額報告
    reportJobs =
        -- 資産取得の実行より1分遅らせる仕掛け
        map (timedelta 60 . Scheduling.packZonedTimeJob)
        [ (t, MatsuiCoJp.Broker.reportCurrentAssets conf)     -- 現在資産評価額報告関数
        | t<-Lib.every (Conf.sendReportInterval conf * 60) times
        ]
    -- |
    -- UTC時間の加減算
    timedelta seconds =
        A.first $ Tm.addUTCTime (fromInteger seconds)

-- |
-- アプリケーションの本体
applicationBody :: CommandLineOption -> Conf.Info -> IO ()
applicationBody cmdLineOpts conf =
    case coptRunMode cmdLineOpts of
    -- 強制的にバッチ処理スレッドを起動する
    RunBatch -> do
        -- バッチ処理の起動時の挨拶文をSlackへ送る
        toSlack (Conf.slack conf) "今回はバッチ処理のみで終了します。"
        batchProcessing conf
    -- 通常処理を開始する
    RunNormal -> do
        -- 起動時の挨拶文をSlackへ送る
        toSlack (Conf.slack conf) Lib.greetingsMessage

--        MatsuiCoJp.Broker.reportSecuritiesAnnounce conf
        MatsuiCoJp.Broker.reportCurrentAssets conf
        --
        -- メインループ
        --
        M.forever $ do
            -- 今日(日本時間)
            jstDay <-   Tm.localDay
                        . Tm.zonedTimeToLocalTime
                        . Tm.utcToZonedTime Lib.tzJST
                        <$> Tm.getCurrentTime
            -- 今日の作業スレッドを実行する
            ths <- M.mapM CC.async $ todayWorks conf jstDay
            --
            -- 作業スレッドの終了を待つ
            -- 作業スレッドの異常終了を確認したら
            -- 残りの作業スレッドをキャンセルして
            -- 例外再送出する
            M.forM_ ths $
                CC.waitCatch M.>=>
                either
                (\ex -> M.forM_ ths CC.cancel >> Ex.throwIO ex)
                return
        --
        -- foreverによって繰り返す
        --
    `Ex.catches`
    -- ユーザー例外ハンドラ
    [ Ex.Handler $ \Ex.UserInterrupt ->
        toSlack (Conf.slack conf) "User Interrupt (pressed Ctrl-C) caught"
    --
    -- 全ての例外ハンドラ
    , Ex.Handler $ \(Ex.SomeException ex) -> do
        -- Slackへエラーメッセージを送る
        let msg = TB.toLazyText
                    $ "Some exception caught, "
                    <> TB.singleton '\"' <> TB.fromString (show ex) <> TB.singleton '\"'
        toSlack (Conf.slack conf) msg
        -- 一定時間待機後に実行時例外からの再開
        CC.threadDelay (300 * 1000 * 1000)
        applicationBody cmdLineOpts conf
    ]

-- |
-- 今日の作業リスト
todayWorks :: Conf.Info -> Tm.Day -> [IO ()]
todayWorks conf day =
    case Tm.toWeekDate day of
     (_,_,w)
        | w == 1 -> weekday     -- Monday
        | w == 2 -> weekday     -- Tuesday
        | w == 3 -> weekday     -- Wednesday
        | w == 4 -> weekday     -- Thursday
        | w == 5 -> weekday     -- Friday
        | w == 6 -> holiday     -- Saturday
        | w == 7 -> holiday     -- Sunday
        | otherwise -> holiday  -- ???
    where
    -- |
    -- 前場のアクション
    morningSession jstDay =
        tradingTimeThread conf . fst $ Scheduling.tradingTimeOfTSEInJST jstDay
    -- |
    -- 後場のアクション
    afternoonSession jstDay =
        tradingTimeThread conf . snd $ Scheduling.tradingTimeOfTSEInJST jstDay
    -- |
    -- 明日になるまでただ待つアクション
    anchorman jstDay =
        Scheduling.execute $ map Scheduling.packZonedTimeJob
            [(Scheduling.tomorrowMidnightJST jstDay, return ())]
    -- |
    -- 平日のアクション
    weekday =
        [ announceWeekdayThread conf day
        , morningSession day
        , afternoonSession day
        , batchProcessThread conf day
        , anchorman day ]
    -- |
    -- 休日のアクション
    holiday =
        [ announceHolidayThread conf day
        , anchorman day ]

--
-- コマンドラインオプション
--
data ApplicationRunMode = RunNormal | RunBatch deriving Show
data CommandLineOption = CommandLineOption
    { coptRunMode    :: ApplicationRunMode
    , coptConfigFile :: String
    } deriving Show

-- |
-- エントリポイント
main :: IO ()
main = do
    copts <- Opt.execParser (Opt.info commandLineOption mempty)
    -- 設定ファイルを読む
    configure <- Conf.readJSONFile $ coptConfigFile copts
    either
        -- 設定ファイルの読み込みに失敗したら、もう何も出来ません
        (TL.putStrLn . TL.pack)
        -- アプリケーションの実行を開始する
        (applicationBody copts)
        configure
    where
    --
    --
    commandLineOption :: Opt.Parser CommandLineOption
    commandLineOption = CommandLineOption
        Opt.<$> Opt.flag RunNormal RunBatch
            (  Opt.long "batch"
            S.<> Opt.help "Perform batch process now"
            )
        Opt.<*> Opt.strOption
            ( Opt.long "conf"
            S.<> Opt.help "Read configuration file"
            S.<> Opt.metavar "CONFIG_FILE"
            S.<> Opt.value "conf.json" -- default file path
            S.<> Opt.help "config file path"
            )

