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
import           Control.Exception
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as M
import           Data.Conduit                 (($$), ($=))
import qualified Data.Conduit                 as C
import           Data.Monoid                  ((<>))
import qualified Data.Semigroup               as S
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TL
import qualified Data.Text.Lazy.IO            as TL
import qualified Data.Time                    as Tm
import qualified Data.Time.Calendar.WeekDate  as Tm
import qualified Network.URI                  as N
import qualified Options.Applicative          as Opt
import qualified Safe

import qualified Aggregate
import qualified Conf
import qualified DataBase
import qualified Lib
import qualified Scheduling
import qualified Scraper
import qualified SinkSlack                    as Slack
import qualified StockQuotesCrawler           as Q
import qualified WebBot

-- | レポートをsinkSlackで送信する形式に変換する関数
reportMsg :: M.MonadIO m => Conf.Info -> C.Conduit Slack.Report m Slack.WebHook
reportMsg = Slack.reportMsg . Conf.slack

-- | 組み立てられたメッセージをConf.Infoで指定されたSlackへ送る関数
sinkSlack :: Conf.Info -> C.Sink Slack.WebHook IO ()
sinkSlack = Slack.sink . Conf.slack

-- | テキストメッセージをConf.InfoSlackで指定されたslackへ送る関数
toSlack :: Conf.InfoSlack -> TL.Text -> IO ()
toSlack conf msg =
    C.yield msg
    $= Slack.simpleTextMsg conf
    $$ Slack.sink conf

-- | テキストメッセージをsinkSlackで送信する形式に変換する関数
simpleTextMsg :: M.MonadIO m => Conf.Info -> C.Conduit TL.Text m Slack.WebHook
simpleTextMsg = Slack.simpleTextMsg . Conf.slack

-- | runResourceTと組み合わせて証券会社のサイトにログイン/ログアウトする
siteConn :: (Monad m, M.MonadTrans t, M.MonadResource (t m)) =>
              Conf.Info -> (WebBot.HTTPSession -> m b) -> t m b
siteConn conf f =
    M.allocate login WebBot.logout
    >>= (\(_,session) -> M.lift $ f session)
    where
    -- | 証券会社のサイトにログインする関数
    login = do
        let url = Conf.loginURL conf
        u <- fail2Throw (url ++ " は有効なURLではありません") $ N.parseURI url
        s <- WebBot.login conf u
        fail2Throw (url ++ " にログインできませんでした") s
    --
    fail2Throw :: String -> Maybe a -> IO a
    fail2Throw  _ (Just x) = return x
    fail2Throw msg Nothing = throwIO (userError msg)

-- | Slackへお知らせを送るついでに現在資産評価をDBへ
reportSecuritiesAnnounce :: Conf.Info -> IO ()
reportSecuritiesAnnounce conf =
    M.runResourceT . siteConn conf $ \session -> do
        -- ホーム -> お知らせを見に行く
        fha <- WebBot.fetchFraHomeAnnounce session
        -- 現在資産評価を証券会社のサイトから取得してDBへ
        fetchPriceToStore session
        -- Slackへお知らせを送る
        C.yield (TL.pack $ show fha) $= simpleTextMsg conf $$ sinkSlack conf

-- | DBから最新の資産評価を取り出してSlackへレポートを送る
reportCurrentAssets :: Conf.Info -> IO ()
reportCurrentAssets conf = do
    -- DBから最新の資産評価を取り出す
    currents <- DataBase.getTotalAstsDescList Nothing 1 0
    M.mapM_ toReport currents
    where
    -- | Slackへレポートを送る関数
    toReport :: DataBase.TotalAssets -> IO ()
    toReport current = do
        -- 前営業日終わりの資産評価(立ち会い開始時間以前の情報)を取り出す
        (Tm.ZonedTime lt tz) <- Tm.getZonedTime
        let prevUTCTime = Tm.localTimeToUTC tz $
                            lt {Tm.localTimeOfDay = Tm.TimeOfDay 9 00 00}
        -- DBより前営業日終了時点の資産評価を取り出す
        prevs <- DataBase.getTotalAstsDescList (Just ("<", prevUTCTime)) 1 0
        let prev = Safe.headMay prevs
        -- 現在値
        let curAsset = DataBase.totalAssetsOfCash current
        -- 前営業日値
        let prvAsset = DataBase.totalAssetsOfCash <$> prev
        -- 前営業日値よりの差
        let diffAsset = (\prv -> curAsset - prv) <$> prvAsset
        -- 最新の資産評価の記録時間
        let tm = DataBase.totalAssetsDateTime current
        -- DBから保有株式を取り出す
        holdStocks <- DataBase.getHoldStockDescList $ Just ("==", tm)
        -- レポートを送る
        let report = Slack.Report {
            Slack.rTime              = tm,
            Slack.rTotalAsset        = curAsset,
            Slack.rAssetDiffByDay    = diffAsset,
            Slack.rTotalProfit       = DataBase.totalAssetsProfit current,
            Slack.rHoldStocks        = holdStocks
        }
        C.yield report $= reportMsg conf $$ sinkSlack conf

-- | 現在資産評価を証券会社のサイトから取得してDBへ
fetchPriceToStore :: WebBot.HTTPSession -> IO ()
fetchPriceToStore session = do
    -- 資産状況 -> 余力情報を見に行く
    spare <- WebBot.fetchFraAstSpare session
    -- 株式取引 -> 現物売を見に行く
    sell <- WebBot.fetchFraStkSell session
    -- 現在時間をキーに全てをデーターベースへ
    M.liftIO $ do
        tm <- Tm.getCurrentTime
        Scraper.storeToDB tm spare
        Scraper.storeToDB tm sell

-- | バッチ処理関数
batchProcessing :: Conf.Info -> IO ()
batchProcessing conf = do
    -- Webクローリング
    Q.runWebCrawlingPortfolios conf
        $= Slack.simpleTextMsg (Conf.slack conf)
        $$ Slack.sink (Conf.slack conf)
    -- 集計
    Aggregate.runAggregateOfPortfolios conf
        $= Slack.simpleTextMsg (Conf.slack conf)
        $$ Slack.sink (Conf.slack conf)

-- | バッチ処理スレッド
batchProcessThread :: Conf.Info -> Tm.Day -> IO ()
batchProcessThread conf jstDay =
    Scheduling.execute $ map Scheduling.packZonedTimeJob
        [(t, batchProcessing conf)
        | t<-Scheduling.batchProcessTimeInJST jstDay
        ]

-- | 平日の報告スレッド
announceWeekdayThread :: Conf.Info -> Tm.Day -> IO ()
announceWeekdayThread conf jstDay =
    Scheduling.execute $ map Scheduling.packZonedTimeJob
        [(t, reportSecuritiesAnnounce conf)
        | t<-Scheduling.announceWeekdayTimeInJST jstDay
        ]

-- | 休日の報告スレッド
announceHolidayThread :: Conf.Info -> Tm.Day -> IO ()
announceHolidayThread conf jstDay =
    Scheduling.execute $ map Scheduling.packZonedTimeJob
        [(t, announce)
        | t<-Scheduling.announceHolidayTimeInJST jstDay
        ]
    where
    --
    msg =
        "本日 " <> TL.fromString (show jstDay) <> " は市場の休日です。"
    --
    announce =
        C.yield (TL.toLazyText msg) $= simpleTextMsg conf $$ sinkSlack conf


-- | 立会時間中のスレッド
tradingTimeThread :: Conf.Info -> [Tm.ZonedTime] -> IO ()
tradingTimeThread conf times =
    Scheduling.execute
        -- 3分前にログインする仕掛け
        $ map (timedelta (-180) . Scheduling.packZonedTimeJob)
        [ (t, worker) | t<-Lib.every (30*60) times ]
        -- ^ 再復帰は30分間隔
    where
    -- 実際の作業
    worker =
        M.runResourceT . siteConn conf $ \sess ->
            let js= fetchPriceJobs sess
                    ++ reportJobs
            in
            Scheduling.execute js
        `catches`
        --
        -- 実行時例外 : 予想外のHTML
        [ Handler $ \(WebBot.UnexpectedHTMLException ex) ->
            let msg = "予想外のHTML例外 \""
                        <> TL.fromString ex
                        <> "\""
            in
            -- Slackへエラーメッセージを送る
            toSlack (Conf.slack conf) $ TL.toLazyText msg
        --
        -- 実行時例外 : 未保有株の売却指示
        , Handler $ \(WebBot.DontHaveStocksToSellException ex) ->
            let msg = "未保有株の売却指示 \""
                        <> TL.fromString ex
                        <> "\""
            in
            -- Slackへエラーメッセージを送る
            toSlack (Conf.slack conf) $ TL.toLazyText msg
        ]
    --
    -- 現在資産取得
    fetchPriceJobs session =
        map Scheduling.packZonedTimeJob
        [ (t, fetchPriceToStore session)    -- 現在資産取得関数
        | t<-Lib.every (Conf.recordAssetsInterval conf * 60) times
        ]
    --
    -- | 現在資産評価額報告
    reportJobs =
        -- 資産取得の実行より1分遅らせる仕掛け
        map (timedelta 60 . Scheduling.packZonedTimeJob)
        [ (t, reportCurrentAssets conf)     -- 現在資産評価額報告関数
        | t<-Lib.every (Conf.sendReportInterval conf * 60) times
        ]
    --
    -- | UTC時間の加減算
    timedelta seconds =
        A.first $ Tm.addUTCTime (fromInteger seconds)

-- | アプリケーションの本体
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
            -- 今日(日本時間)
            jstDay <-   Tm.localDay
                        . Tm.zonedTimeToLocalTime
                        . Tm.utcToZonedTime Lib.tzJST
                        <$> Tm.getCurrentTime
            -- 今日の作業スレッドリスト
            let works = case Tm.toWeekDate jstDay of
                        (_,_,w) | w == 1 -> weekday     -- Monday
                                | w == 2 -> weekday     -- Tuesday
                                | w == 3 -> weekday     -- Wednesday
                                | w == 4 -> weekday     -- Thursday
                                | w == 5 -> weekday     -- Friday
                                | w == 6 -> holiday     -- Saturday
                                | w == 7 -> holiday     -- Sunday
                                | otherwise -> holiday  -- ???
            -- 今日の作業スレッドを実行する
            ths <- M.mapM (CC.async . (\f -> f jstDay)) works
            -- 作業スレッドの異常終了を確認したら再起動する
            (_, result) <- CC.waitAnyCatchCancel ths
            case result of
                -- 正常終了
                Right _ -> return ()
                -- 失敗
                Left ex -> do
                    let msg = "Some exception caught, \""
                                <> TL.fromString (show ex)
                                <> "\""
                    -- Slackへエラーメッセージを送る
                    toSlack (Conf.slack conf) $ TL.toLazyText msg
        --
        -- foreverによって繰り返す
        --
    `catches`
        -- ユーザー例外ハンドラ
        [ Handler $ \UserInterrupt ->
            toSlack (Conf.slack conf) "User Interrupt (pressed Ctrl-C) caught"
        --
        -- 全ての例外ハンドラ
        , Handler $ \(SomeException ex) -> do
            let msg = "Some exception caught, \""
                        <> TL.fromString (show ex)
                        <> "\""
            -- Slackへエラーメッセージを送る
            toSlack (Conf.slack conf) $ TL.toLazyText msg
            -- 一定時間待機後に実行時例外からの再開
            CC.threadDelay (300 * 1000 * 1000)
            applicationBody cmdLineOpts conf
        ]
    where
    -- | 前場のスレッド
    morningSession jstDay =
        tradingTimeThread conf . fst $ Scheduling.tradingTimeOfTSEInJST jstDay
    --
    -- | 後場のスレッド
    afternoonSession jstDay =
        tradingTimeThread conf . snd $ Scheduling.tradingTimeOfTSEInJST jstDay
    --
    -- | 明日になるまでただ待つスレッド
    anchorman jstDay =
        Scheduling.execute $ map Scheduling.packZonedTimeJob
            [(Scheduling.tomorrowMidnightJST jstDay, return ())]
    --
    weekday =
        [ announceWeekdayThread conf
        , morningSession
        , afternoonSession
        , batchProcessThread conf
        , anchorman
        ]
    --
    holiday =
        [ announceHolidayThread conf, anchorman ]

--
data ApplicationRunMode = RunNormal | RunBatch deriving Show
data CommandLineOption = CommandLineOption
    { coptRunMode    :: ApplicationRunMode
    , coptConfigFile :: String
    } deriving Show

-- | エントリポイント
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
    commandLineOption :: Opt.Parser CommandLineOption
    commandLineOption = CommandLineOption
        Opt.<$> Opt.flag RunNormal RunBatch
            (  Opt.long "batch"
            S.<> Opt.help "forced run batch processing"
            )
        Opt.<*> Opt.strOption
            ( Opt.long "conf"
            S.<> Opt.help "forced run batch processing"
            S.<> Opt.metavar "CONFIG_FILE"
            S.<> Opt.value "conf.json" -- default file path
            S.<> Opt.help "config file path"
            )

