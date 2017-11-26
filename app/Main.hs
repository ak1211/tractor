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
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Safe
import Control.Exception hiding (throw)

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Reader as M
import Data.Monoid ((<>))

import Data.Conduit (($$), ($=))
import qualified Data.Conduit as C

import qualified Network.URI as N

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Data.Time as Tm
import qualified Control.Concurrent as CC

import qualified Options.Applicative as Opt
import qualified Data.Semigroup as S

import qualified Conf
import qualified WebBot
import qualified Scraper
import qualified DataBase
import qualified SinkSlack as Slack
import qualified StockQuotesCrawler as Q
import qualified Aggregate
import qualified Lib

--
fixedHourMinutes :: Tm.ZonedTime -> Int
fixedHourMinutes ztm =
    let t = Tm.localTimeOfDay (Tm.zonedTimeToLocalTime ztm) in
    Tm.todHour t*100 + Tm.todMin t
        --
-- | お知らせの配信時間か？
isDeliverAnnounceTime :: Tm.ZonedTime -> Bool
isDeliverAnnounceTime ztm = fixedHourMinutes ztm `elem` [630, 1135, 1515]

-- | バッチ処理の開始時間か？
isBatchThreadWorkingTime :: Tm.ZonedTime -> Bool
isBatchThreadWorkingTime ztm = fixedHourMinutes ztm `elem` [1700]

-- | プログラムの作業時間か？
isDuringWorkingTime :: Tm.ZonedTime -> Bool
isDuringWorkingTime ztm =
    fixedHourMinutes ztm `elem` ([900 .. 1130] ++ [1230 .. 1500])

-- | 証券会社のサイトにログインする関数
loginToSecuritiesSite :: Conf.Info -> IO WebBot.HTTPSession
loginToSecuritiesSite conf = do
    let url = Conf.loginURL conf
    u <- fail2Throw (url ++ " は有効なURLではありません") $ N.parseURI url
    s <- WebBot.login conf u
    fail2Throw (url ++ " にログインできませんでした") s
    where
    --
    fail2Throw :: String -> Maybe a -> IO a
    fail2Throw  _ (Just x) = return x
    fail2Throw msg Nothing = throwIO (userError msg)

-- | テキストメッセージをConf.InfoSlackで指定されたslackへ送る関数
toSlack :: Conf.InfoSlack -> TL.Text -> IO ()
toSlack conf msg =
    C.yield msg
    $= Slack.simpleTextMsg conf
    $$ Slack.sink conf

-- | テキストメッセージをsinkSlackで送信する形式に変換する関数
simpleTextMsg :: M.MonadIO m => Conf.Info -> C.Conduit TL.Text m Slack.WebHook
simpleTextMsg = Slack.simpleTextMsg . Conf.slack

-- | レポートをsinkSlackで送信する形式に変換する関数
reportMsg :: M.MonadIO m => Conf.Info -> C.Conduit Slack.Report m Slack.WebHook
reportMsg = Slack.reportMsg . Conf.slack

-- | 組み立てられたメッセージをConf.Infoで指定されたSlackへ送る関数
sinkSlack :: Conf.Info -> C.Sink Slack.WebHook IO ()
sinkSlack = Slack.sink . Conf.slack

-- | 現在資産評価を証券会社のサイトから取得してDBへ
recordCurrentCondition :: M.ReaderT WebBot.HTTPSession IO ()
recordCurrentCondition = do
    -- 資産状況 -> 余力情報を見に行く
    spare <- WebBot.fetchFraAstSpare
    -- 株式取引 -> 現物売を見に行く
    sell <- WebBot.fetchFraStkSell
    -- 現在時間をキーに全てをデーターベースへ
    M.liftIO $ do
        tm <- Tm.getCurrentTime
        Scraper.storeToDB tm spare
        Scraper.storeToDB tm sell

-- | Slackへお知らせを送るついでに現在資産評価をDBへ
sendAnnounce :: Conf.Info -> IO ()
sendAnnounce conf = do
    -- 証券会社のサイトにログイン
    session <- loginToSecuritiesSite conf
    -- ホーム -> お知らせを見に行く
    fha <- M.runReaderT WebBot.fetchFraHomeAnnounce session
    -- 現在資産評価を証券会社のサイトから取得してDBへ
    M.runReaderT recordCurrentCondition session
    -- Slackへお知らせを送る
    C.yield (TL.pack $ show fha) $= simpleTextMsg conf $$ sinkSlack conf
    -- 証券会社のサイトからログアウト
    WebBot.logout session

-- | DBから最新の資産評価を取り出してSlackへレポートを送る
reportOnCurrentCondition :: Conf.Info -> IO ()
reportOnCurrentCondition conf = do
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

-- | バッチ処理
batchProcessing :: Conf.Info -> IO ()
batchProcessing conf = do
    Q.runWebCrawlingOfPortfolios conf
        $= Slack.simpleTextMsg (Conf.slack conf)
        $$ Slack.sink (Conf.slack conf)
    Aggregate.runAggregateOfPortfolios conf
        $= Slack.simpleTextMsg (Conf.slack conf)
        $$ Slack.sink (Conf.slack conf)

-- | スレッドへ送る指示
data ThMsgSig
    = Run           -- ^ 作業開始指示
    | RunIsOver     -- ^ 作業終了指示
    | Donothing     -- ^ スレッド終了状態

-- | スレッドへ指示を送る
sendMsgSig :: CC.MVar ThMsgSig -> ThMsgSig -> IO ()
sendMsgSig mBox message =
    mask_ (CC.takeMVar mBox >> CC.putMVar mBox message)

-- | Slackへお知らせを送るスレッド
sendAnnounceThread :: Conf.Info -> CC.ThreadId -> CC.MVar ThMsgSig -> IO ()
sendAnnounceThread conf parentThId msgBox =
    do
        -- readMVarによって指示があるまで待機する
        msg <- CC.readMVar msgBox
        case msg of
            -- 作業開始指示が来た
            Run -> do
                -- Slackへお知らせを送る
                sendAnnounce conf
                -- このスレッドの実行時間は必ず１分以上かかるようにする
                Lib.doSleepThread 1
            -- 作業開始指示以外ならスレッドを終了する
            RunIsOver -> return ()
            Donothing -> return ()
    `catch`
        -- 例外は親スレッドに再送出
        \(SomeException e) -> do
            sendMsgSig msgBox Donothing
            throwTo parentThId e

-- | バッチ処理スレッド
batchProcessingThread :: Conf.Info -> CC.ThreadId -> CC.MVar ThMsgSig -> IO ()
batchProcessingThread conf _ msgBox =
    do
        -- readMVarによって指示があるまで待機する
        msg <- CC.readMVar msgBox
        case msg of
            -- 作業開始指示が来た
            Run -> batchProcessing conf
            -- 作業開始指示以外ならスレッドを終了する
            RunIsOver -> return ()
            Donothing -> return ()
    `catch`
    -- このスレッドでは例外は握りつぶす
    \(SomeException ex) -> do
        sendMsgSig msgBox Donothing
        let msg = "[batch process] exception caught, \"" <> TL.fromString (show ex) <> "\""
        -- Slackへエラーメッセージを送る
        toSlack (Conf.slack conf) $ TL.toLazyText msg

-- | Slackへレポートを送るスレッド
sendReportThread :: Conf.Info -> CC.ThreadId -> CC.MVar ThMsgSig -> IO ()
sendReportThread conf parentThId msgBox =
    -- readMVarによって指示があるまで待機する
    (loop 0 =<< CC.readMVar msgBox)
    `catch`
    -- 例外は親スレッドに再送出
    \(SomeException e) -> do
        sendMsgSig msgBox Donothing
        throwTo parentThId e
    where
    loop :: Int -> ThMsgSig -> IO ()
    -- 作業開始指示が来た
    loop remain Run
        | remain <= 0 = do
            -- Slackへレポートを送る
            reportOnCurrentCondition conf
            -- ループ
            loop (Conf.sendReportInterval conf) =<< CC.readMVar msgBox
        | otherwise = do
            -- 再開までの時間待ち
            Lib.doSleepThread 1
            loop (remain - 1) =<< CC.readMVar msgBox
    -- 作業終了指示が来たのでスレッドを終了する
    loop _ RunIsOver = sendMsgSig msgBox Donothing
    -- Donothingは終了状態を表しているのでその通りスレッドを終了する
    loop _ Donothing = return ()

-- | 現在資産評価をDBへ格納するスレッド
recordAssetsThread :: Conf.Info -> CC.ThreadId -> CC.MVar ThMsgSig -> IO ()
recordAssetsThread conf parentThId msgBox =
    do
        -- readMVarによって指示があるまで待機する
        msg <- CC.readMVar msgBox
        -- 証券会社のサイトにログイン
        session <- loginToSecuritiesSite conf
        --
        loop 0 msg session
        -- 証券会社のサイトからログアウト
        WebBot.logout session
    `catch`
        -- 例外は親スレッドに再送出
        \(SomeException e) -> do
            sendMsgSig msgBox Donothing
            throwTo parentThId e
    where
    loop :: Int -> ThMsgSig -> WebBot.HTTPSession -> IO ()
    -- 作業開始指示が来た
    loop remain Run session
        | remain <= 0 = do
            -- 現在資産評価を取得してDBへ
            M.runReaderT recordCurrentCondition session
            -- メッセージの確認をする
            msg <- CC.readMVar msgBox
            -- ループ
            loop (Conf.recordAssetsInterval conf) msg session
        | otherwise = do
            -- 再開までの時間待ち
            Lib.doSleepThread 1
            -- メッセージの確認をする
            msg <- CC.readMVar msgBox
            loop (remain - 1) msg session
    -- 作業終了指示が来たのでスレッドを終了する
    loop _ RunIsOver _ = sendMsgSig msgBox Donothing
    -- Donothingは終了状態を表しているのでその通りスレッドを終了する
    loop _ Donothing _ = return ()

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
        -- 具体的な処理を担当する関数と
        -- MVarのタプルを用意する
        myThId <- CC.myThreadId
        threads <- M.mapM
                    -- (MVar, function)のタプルを作る
                    (\fn -> curry id <$> CC.newMVar Donothing <*> pure fn)
                    [ recordAssetsThread conf myThId
                    , sendReportThread conf myThId ]
        -- メインループ
        M.forever (loop threads)
    `catch`
    -- 全ての例外ハンドラ
    \(SomeException ex) -> do
        let msg = "Some exception caught, \"" <> TL.fromString (show ex) <> "\""
        -- Slackへエラーメッセージを送る
        toSlack (Conf.slack conf) $ TL.toLazyText msg
        -- 一定時間待機後に実行時例外からの再開
        Lib.doSleepThread 11
        applicationBody cmdLineOpts conf
    where
    loop :: [(CC.MVar ThMsgSig, CC.MVar ThMsgSig -> IO ())] -> IO ()
    loop threads = do
        Tm.getZonedTime >>= \case
            ztm | isDeliverAnnounceTime ztm -> do
                    -- お知らせの配信時間
                    myThId <- CC.myThreadId
                    mBox <- CC.newMVar Run
                    M.void . CC.forkIO . sendAnnounceThread conf myThId $ mBox
                | isBatchThreadWorkingTime ztm -> do
                    -- バッチ処理スレッドを起動する
                    myThId <- CC.myThreadId
                    mBox <- CC.newMVar Run
                    M.void . CC.forkIO . batchProcessingThread conf myThId $ mBox
                | isDuringWorkingTime ztm ->
                    -- 立会時間中
                    -- スレッドに作業開始指示を送る
                    M.mapM_ sendSigRun threads
                | otherwise ->
                    -- 立会時間外
                    -- スレッドに作業終了指示を送る
                    M.mapM_ sendSigRunIsOver threads
        -- このスレッドは作業の担当スレッドに
        -- 指示することしかしないので適当に時間をつぶす
        Lib.doSleepThread 1
        `onException`
            -- 子スレッドに作業終了指示を送っておく
            M.mapM_ sendSigRunIsOver threads

    -- | スレッドに作業開始指示を送る関数
    --   スレッドが終了していたなら起動する
    sendSigRun :: (CC.MVar ThMsgSig, CC.MVar ThMsgSig -> IO ()) -> IO ()
    sendSigRun (mBox, fnc) =
        CC.readMVar mBox >>= \case
            -- スレッドが終了していたなら起動して
            -- 作業開始指示を送る
            Donothing -> do
                sendMsgSig mBox Run
                M.void $ CC.forkIO (fnc mBox)
                -- 起動タイミングをずらすために28.657秒待つ
                CC.threadDelay (28657 * 1000)
            -- 作業開始指示または
            -- 作業終了指示が送られている場合はそのままにする
            Run -> return ()
            RunIsOver -> return ()

    -- | スレッドに作業終了指示を送る関数
    sendSigRunIsOver :: (CC.MVar ThMsgSig, CC.MVar ThMsgSig -> IO ()) -> IO ()
    sendSigRunIsOver (mBox, _) =
        CC.readMVar mBox >>= \case
            -- スレッドが終了していたなら書き換えずにそのままにする
            Donothing -> return ()
            -- 作業終了指示を送る
            Run -> sendMsgSig mBox RunIsOver
            RunIsOver -> sendMsgSig mBox RunIsOver

--
data ApplicationRunMode = RunNormal | RunBatch deriving Show
data CommandLineOption = CommandLineOption
    { coptRunMode       :: ApplicationRunMode
    , coptConfigFile    :: String
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

