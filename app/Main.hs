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
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

アプリケーション「Tractor」のメインモジュールです。
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Main where
import qualified Control.Concurrent           as CC
import qualified Control.Concurrent.Async     as CCA
import qualified Control.Exception            as Ex
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Trans.Resource as M
import           Data.Conduit                 (($$), ($=), (=$))
import qualified Data.Conduit                 as C
import           Data.Monoid                  ((<>))
import qualified Data.Semigroup               as Sg
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TB
import qualified Data.Text.Lazy.IO            as TL
import qualified Data.Time                    as Tm
import qualified Data.Time.Calendar.WeekDate  as Tm
import qualified Database.Persist.MySQL       as MySQL
import qualified Options.Applicative          as Opt

import qualified Aggregate
import qualified BrokerBackend                as BB
import qualified Conf
import qualified GenBroker                    as GB
import qualified Lib
import qualified Scheduling                   as Scd
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
    webCrawling <> aggregate $$ slack
    where
    --
    --
    slack =
        Slack.simpleTextMsg (Conf.slack conf)
        =$ Slack.sink (Conf.slack conf)
    -- |
    -- Webクローリング
    webCrawling = Q.runWebCrawlingPortfolios conf
    -- |
    -- 集計
    aggregate = Aggregate.runAggregateOfPortfolios conf

-- |
-- バッチ処理スケジュール
batchProcessSchedule :: Conf.Info -> Scd.AsiaTokyoDay -> Scd.ZonedTimeJobs
batchProcessSchedule conf day =
    [(t, act) | t<-Scd.batchProcessTime day]
    where
    act = batchProcessing conf

-- |
-- 平日の報告スケジュール
announceWeekdaySchedule :: Conf.Info -> Scd.AsiaTokyoDay -> Conf.InfoBroker -> Scd.ZonedTimeJobs
announceWeekdaySchedule conf day broker =
    [(t, act) | t<-Scd.announceWeekdayTime day]
    where
    act = GB.noticeOfBrokerageAnnouncement broker connInfo ua $$ slack
    slack = simpleTextMsg conf =$ sinkSlack conf
    ua = Conf.userAgent conf
    --
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

-- |
-- 休日の報告スケジュール
announceHolidaySchedule :: Conf.Info -> Scd.AsiaTokyoDay -> Scd.ZonedTimeJobs
announceHolidaySchedule conf day =
    [(t, act) | t<-Scd.announceHolidayTime day]
    where
    act = C.yield (TB.toLazyText msg) $$ slack
    msg = "本日 " <> today <> " は市場の休日です。"
    today = TB.fromString . show $ Scd.getAsiaTokyoDay day
    slack = simpleTextMsg conf =$ sinkSlack conf

-- |
-- 立会時間のスケジュール
--
tradingTimeSchedule :: Conf.Info -> Conf.InfoBroker -> [Tm.ZonedTime] -> Scd.ZonedTimeJobs
tradingTimeSchedule conf broker times =
    -- 3分前にログインする仕掛け
    map (Scd.addTimeOfSecondsZT (-180)) resumableJob
    where
    --
    --
    resumableJob :: Scd.ZonedTimeJobs
    resumableJob =
        [(t, onePass) | t<-Lib.every (30*60) times]  -- 失敗による停止後の再復帰は30分間隔
    --
    -- 実際の作業
    onePass :: IO ()
    onePass =
        M.runResourceT . GB.siteConn broker (Conf.userAgent conf) $ \sess ->
            Scd.executeZT (fetchPriceJobs sess updateSeconds ++ reportJobs noticeSeconds)
        `Ex.catches`
        --
        -- ここの例外ハンドラは例外再送出しないので、
        -- 例外処理の後は関数から抜ける
        -- つまりこのスレッドの終了
        --
        [ Ex.Handler $ \(BB.UnexpectedHTMLException ex) ->
            toSlack (Conf.slack conf) $ toText ex "予想外のHTML例外 "
        --
        , Ex.Handler $ \(BB.DontHaveStocksToSellException ex) ->
            toSlack (Conf.slack conf) $ toText ex "未保有株の売却指示 "
        --
        , Ex.Handler $ \(Ex.SomeException ex) ->
            toSlack (Conf.slack conf) $ toText ex mempty
        ]
    --
    --
    toText ex m =
        TB.toLazyText $ m
        <> TB.singleton '\"' <> TB.fromString (show ex) <> TB.singleton '\"'
    --
    --
    conn = Conf.connInfoDB $ Conf.mariaDB conf
    report = reportMsg conf =$ sinkSlack conf
    -- |
    -- 現在資産取得時間
    updateSeconds =
        Lib.every (Conf.updatePriceMinutes conf * 60) times
    -- |
    -- 現在資産評価額報告時間
    noticeSeconds =
        Lib.every (Conf.noticeAssetsMinutes conf * 60) times
    -- |
    -- 現在資産取得
    fetchPriceJobs :: BB.HTTPSession -> [Tm.ZonedTime] -> Scd.ZonedTimeJobs
    fetchPriceJobs session ts =
        [(t, GB.fetchUpdatedPriceAndStore broker conn session) | t<-ts]
    -- |
    -- 現在資産評価額報告
    reportJobs :: [Tm.ZonedTime] -> Scd.ZonedTimeJobs
    reportJobs ts =
        -- 同時間にならないために資産取得の実行より1分遅らせる仕掛け
        map (Scd.addTimeOfSecondsZT 60)
        [(t, GB.noticeOfCurrentAssets broker conn $$ report) | t<-ts]

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
        --
        -- メインループ
        --
        M.forever $ do
            assign <- toAssignThreads conf <$> getToday
            -- 今日の作業スレッドを実行する
            threads <- M.mapM (CCA.async . Scd.executeZT) assign
            --
            -- 作業スレッドの終了を待つ
            -- 作業スレッドの異常終了を確認したら
            -- 残りの作業スレッドをキャンセルして
            -- 例外再送出する
            M.forM_ threads $
                CCA.waitCatch M.>=>
                either
                (\ex -> M.forM_ threads CCA.cancel >> Ex.throwIO ex)
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
                    $ "Some exception caught (at mainloop), "
                    <> TB.singleton '\"' <> TB.fromString (show ex) <> TB.singleton '\"'
        toSlack (Conf.slack conf) msg
        -- 一定時間待機後に実行時例外からの再開
        CC.threadDelay (300 * 1000 * 1000)
        applicationBody cmdLineOpts conf
    ]
    where
    -- 今日(日本時間)
    getToday :: IO Scd.AsiaTokyoDay
    getToday =
        Scd.AsiaTokyoDay
        . Tm.localDay
        . Tm.zonedTimeToLocalTime
        . Tm.utcToZonedTime Lib.tzAsiaTokyo
        <$> Tm.getCurrentTime

-- |
-- 今日の実行スレッドに割り当てるリスト
toAssignThreads :: Conf.Info -> Scd.AsiaTokyoDay -> [Scd.ZonedTimeJobs]
toAssignThreads conf day =
    case Tm.toWeekDate $ Scd.getAsiaTokyoDay day of
     (_,_,w)
        | w == 1 -> weekday     -- Monday
        | w == 2 -> weekday     -- Tuesday
        | w == 3 -> weekday     -- Wednesday
        | w == 4 -> weekday     -- Thursday
        | w == 5 -> weekday     -- Friday
        | w == 6 -> holiday     -- Saturday
        | w == 7 -> holiday     -- Sunday
        | otherwise -> holiday  -- ???
    ++
    -- これ以降は明日になるまでの待ち
    [anchorman]
    where
    --
    -- 前場後場の立会時間(東京証券取引所,日本時間)
    morSessSeconds :: [Tm.ZonedTime]
    aftSessSeconds :: [Tm.ZonedTime]
    (morSessSeconds, aftSessSeconds) = Scd.tradingTimeOfSecondsTSE day
    -- |
    -- 明日になるまでただ待つアクション
    anchorman =
        let nop = return () in
        [(Scd.tomorrowMidnight day, nop)]
    -- |
    -- 平日のアクション
    weekday =
        let
            ann = announceWeekdaySchedule conf day
            mor broker = tradingTimeSchedule conf broker morSessSeconds
            aft broker = tradingTimeSchedule conf broker aftSessSeconds
        in
        -- 証券会社毎に割り当てる
        concatMap (`map` Conf.brokers conf) [ann, mor, aft]
        ++
        [batchProcessSchedule conf day]
    -- |
    -- 休日のアクション
    holiday =
        [announceHolidaySchedule conf day]

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
            Sg.<> Opt.help "Perform batch process now"
            )
        Opt.<*> Opt.strOption
            ( Opt.long "conf"
            Sg.<> Opt.help "Read configuration file"
            Sg.<> Opt.metavar "CONFIG_FILE"
            Sg.<> Opt.value "conf.json" -- default file path
            Sg.<> Opt.help "config file path"
            )

