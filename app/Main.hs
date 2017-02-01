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
Module      :  Main.hs
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

import qualified Conf
import WebBot (UnexpectedHTMLException)
import qualified WebBot
import qualified Scraper
import qualified DataBase
import qualified SlackBot

import Prelude hiding (catch)
import Control.Exception hiding (throw)
import Control.Monad.Trans.Resource (runResourceT)

import Control.Applicative ((<$>), (<*>), (<*))
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Reader as M
import Data.Conduit (Source, ($$), ($=), yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Network.URI as N
import qualified Network.HTTP.Types.Status as N
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Text.Printf as Printf

import Data.Fixed
import qualified Data.Time as Tm
import qualified Data.Time.Calendar as Tm
import qualified Data.Time.LocalTime as LT
import qualified Control.Concurrent as CC
import Control.Concurrent.MVar

import qualified System.Posix.Signals as S

{- |
    起動時の挨拶文
-}
greetingsMessage = T.unlines
    [ "tractorが起動しました"
    , "以降一定時間で通知します。"
    , "tractor is an Assets observation application."
    , "*tractor © 2016, 2017 Akihiro Yamamoto.*"
    , "このプログラムは *全くの無保証* で提供されます。"
    , "これはフリーソフトウェアであり、ある条件の下で再頒布することが奨励されています。"
    , "詳しくは https://github.com/ak1211/tractor をご覧ください。"
    ]

{- |
    時間調整でスレッドを停止する関数
    停止時間の単位は分
-}
doSleep minutes = CC.threadDelay (minutes * 60 * 1000 * 1000)

{- |
    お知らせの配信時間か？
-}
isDeliverAnnounceTime :: LT.ZonedTime -> Bool
isDeliverAnnounceTime ztm =
    hm `elem` [630, 1140, 1510]
    where
    hm =
        let t = LT.localTimeOfDay $ LT.zonedTimeToLocalTime ztm in
        LT.todHour t*100 + LT.todMin t

{- |
    プログラムの作業時間か？
-}
isDuringWorkingTime :: LT.ZonedTime -> Bool
isDuringWorkingTime ztm =
    isMorningSession || isAfternoonSession
    where
    hm =
        let t = LT.localTimeOfDay (LT.zonedTimeToLocalTime ztm) in
        LT.todHour t*100 + LT.todMin t
    isMorningSession   =  900 <= hm && hm <= 1130
    isAfternoonSession = 1230 <= hm && hm <= 1500

{- |
    証券会社のサイトにログインする関数
-}
loginToSecuritiesSite :: Conf.Info -> IO WebBot.HTTPSession
loginToSecuritiesSite conf = do
    let url = Conf.loginURL conf
    u <- fail2Throw (url++" は有効なURLではありません") $ N.parseURI url
    s <- WebBot.login conf u
    fail2Throw (url++" にログインできませんでした") s
    where
    --
    fail2Throw :: String -> Maybe a -> IO a
    fail2Throw  _ (Just x) = return x
    fail2Throw msg Nothing = throwIO (userError msg)

{- |
    テキストメッセージをConf.InfoSlackで指定されたslackへ送る関数
-}
toSlack :: Conf.InfoSlack -> T.Text -> IO ()
toSlack conf msg =
    C.yield msg
    $= SlackBot.simpleTextMsg conf
    $$ SlackBot.sinkSlack conf

{- |
    テキストメッセージをsinkSlackで送信する形式に変換する関数
-}
simpleTextMsg :: (Monad m, M.MonadIO m) => Conf.Info -> C.Conduit T.Text m SlackBot.WebHook
simpleTextMsg = SlackBot.simpleTextMsg . Conf.slack

{- |
    レポートをsinkSlackで送信する形式に変換する関数
-}
reportMsg :: (Monad m, M.MonadIO m) => Conf.Info -> C.Conduit SlackBot.Report m SlackBot.WebHook
reportMsg = SlackBot.reportMsg . Conf.slack

{- |
    組み立てられたメッセージをConf.Infoで指定されたSlackへ送る関数
-}
sinkSlack :: Conf.Info -> C.Sink SlackBot.WebHook IO ()
sinkSlack = SlackBot.sinkSlack . Conf.slack

{- |
    現在資産評価を証券会社のサイトから取得してDBへ
-}
recordCurrentCondition :: M.ReaderT WebBot.HTTPSession IO ()
recordCurrentCondition = do
    session <- M.ask
    -- 資産状況 -> 余力情報を見に行く
    spare <- WebBot.fetchFraAstSpare
    -- 株式取引 -> 現物売を見に行く
    sell <- WebBot.fetchFraStkSell

    M.liftIO $ do
        -- 現在時間をキーに全てをデーターベースへ
        tm <- Tm.getCurrentTime
        Scraper.storeToDB tm spare
        Scraper.storeToDB tm sell

{- |
    Slackへお知らせを送るついでに現在資産評価をDBへ
-}
sendAnnounce :: Conf.Info -> IO ()
sendAnnounce conf = do
    -- 証券会社のサイトにログイン
    session <- loginToSecuritiesSite conf
    -- ホーム -> お知らせを見に行く
    fha <- M.runReaderT WebBot.fetchFraHomeAnnounce session
    -- 現在資産評価を証券会社のサイトから取得してDBへ
    M.runReaderT recordCurrentCondition session
    -- Slackへお知らせを送る
    C.yield (T.pack $ show fha) $= simpleTextMsg conf $$ sinkSlack conf
    -- 証券会社のサイトからログアウト
    WebBot.logout session

{- |
    DBから最新の資産評価を取り出してSlackへレポートを送る
-}
reportOnCurrentCondition :: Conf.Info -> IO ()
reportOnCurrentCondition conf = do
    -- DBから最新の資産評価を取り出す
    currents <- DataBase.getTotalAstsDescList Nothing 1 0

    M.mapM_ toReport currents
    where
    {- |
        Slackへレポートを送る関数
    -}
    toReport :: DataBase.TotalAssets -> IO ()
    toReport current = do
        -- 前営業日終わりの資産評価(立ち会い開始時間以前の情報)を取り出す
        (LT.ZonedTime lt tz) <- LT.getZonedTime
        let prevUTCTime = LT.localTimeToUTC tz $
                            lt {LT.localTimeOfDay = LT.TimeOfDay 9 00 00}
        -- DBより前営業日終了時点の資産評価を取り出す
        prevs <- DataBase.getTotalAstsDescList (Just ("<", prevUTCTime)) 1 0
        let prev = Maybe.listToMaybe prevs
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
        let report = SlackBot.Report {
            SlackBot.rTime              = tm,
            SlackBot.rTotalAsset        = curAsset,
            SlackBot.rAssetDiffByDay    = diffAsset,
            SlackBot.rTotalProfit       = DataBase.totalAssetsProfit current,
            SlackBot.rHoldStocks        = holdStocks
        }
        C.yield report $= reportMsg conf $$ sinkSlack conf

-- | スレッドへ送る指示
data ThMsgSig
    = Run           -- ^ 作業開始指示
    | RunIsOver     -- ^ 作業終了指示
    | Donothing     -- ^ スレッド終了状態

{- |
    スレッドへ指示を送る
-}
sendMsgSig :: MVar ThMsgSig -> ThMsgSig -> IO ()
sendMsgSig mBox message =
    mask_ (takeMVar mBox >> putMVar mBox message)

{- |
    Slackへお知らせを送るスレッド
-}
sendAnnounceThread :: Conf.Info -> CC.ThreadId -> MVar ThMsgSig -> IO ()
sendAnnounceThread conf parentThId msgBox =
    do
        -- readMVarによって指示があるまで待機する
        msg <- readMVar msgBox
        case msg of
            {-
                作業開始指示が来た
            -}
            Run -> do
                -- Slackへお知らせを送る
                sendAnnounce conf
                -- このスレッドの実行時間は必ず１分以上かかるようにする
                doSleep 1
            {-
                作業開始指示以外ならスレッドを終了する
            -}
            _ -> return ()
    `catch`
        -- 例外は親スレッドに再送出
        \(SomeException e) -> do
            sendMsgSig msgBox Donothing
            throwTo parentThId e

{- |
    Slackへレポートを送るスレッド
-}
sendReportThread :: Conf.Info -> CC.ThreadId -> MVar ThMsgSig -> IO ()
sendReportThread conf parentThId msgBox =
    -- readMVarによって指示があるまで待機する
    (loop 0 =<< readMVar msgBox)
    `catch`
        -- 例外は親スレッドに再送出
        \(SomeException e) -> do
            sendMsgSig msgBox Donothing
            throwTo parentThId e
    where
    loop :: Int -> ThMsgSig -> IO ()
    {-
        作業開始指示が来た
    -}
    loop remain Run =
        if remain <= 0
        then do
            -- Slackへレポートを送る
            reportOnCurrentCondition conf
            -- ループ
            loop (Conf.sendReportInterval conf) =<< readMVar msgBox
        else do
            -- 再開までの時間待ち
            doSleep 1
            loop (remain - 1) =<< readMVar msgBox
    {-
        作業終了指示が来たのでスレッドを終了する
    -}
    loop _ RunIsOver = sendMsgSig msgBox Donothing
    {-
        Donothingは終了状態を表しているのでその通りスレッドを終了する
    -}
    loop _ Donothing = return ()

{- |
    現在資産評価をDBへ格納するスレッド
-}
recordAssetsThread :: Conf.Info -> CC.ThreadId -> MVar ThMsgSig -> IO ()
recordAssetsThread conf parentThId msgBox =
    do
        -- readMVarによって指示があるまで待機する
        msg <- readMVar msgBox
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
    {-
        作業開始指示が来た
    -}
    loop remain Run session =
        if remain <= 0
        then do
            -- 現在資産評価を取得してDBへ
            M.runReaderT recordCurrentCondition session
            -- メッセージの確認をする
            msg <- readMVar msgBox
            -- ループ
            loop (Conf.recordAssetsInterval conf) msg session
        else do
            -- 再開までの時間待ち
            doSleep 1
            -- メッセージの確認をする
            msg <- readMVar msgBox
            loop (remain - 1) msg session
    {-
        作業終了指示が来たのでスレッドを終了する
    -}
    loop _ RunIsOver _ = sendMsgSig msgBox Donothing
    {-
        Donothingは終了状態を表しているのでその通りスレッドを終了する
    -}
    loop _ Donothing _ = return ()

{- |
    アプリケーションの本体
-}
applicationBody :: String -> IO ()
applicationBody confFilePath = do
    -- 設定ファイルを読む
    c <- Conf.readJSONFile confFilePath
    case c of
        -- 設定ファイルの読み込みに失敗したら、もう何も出来ません
        Left msg -> T.putStrLn $ T.pack msg
        -- アプリケーションの実行を開始する
        Right conf -> do
            -- 起動時の挨拶文をSlackへ送る
            toSlack (Conf.slack conf) greetingsMessage
            {-
                具体的な処理を担当する関数と
                MVarのタプルを用意する
            -}
            myThId <- CC.myThreadId
            threads <- M.mapM
                        -- (MVar, function)のタプルを作る
                        (\fn -> curry id <$> newMVar Donothing <*> pure fn)
                        [ recordAssetsThread conf myThId
                        , sendReportThread conf myThId ]
            -- メインループ
            M.forever (loop conf threads)
            `catch`
            \(SomeException ex) -> do
                let s = Printf.printf "exception caught, \"%s\"" (show ex)
                -- Slackへエラーメッセージを送る
                toSlack (Conf.slack conf) (T.pack s)
                -- 一定時間待機後に実行時例外からの再開
                doSleep 11
                applicationBody confFilePath
    where
    loop :: Conf.Info -> [(MVar ThMsgSig, MVar ThMsgSig -> IO ())] -> IO ()
    loop conf threads = do
        ztm <- LT.getZonedTime
        case ztm of
            t| isDeliverAnnounceTime t -> do
                {-
                    お知らせの配信時間
                -}
                myThId <- CC.myThreadId
                mBox <- newMVar Run
                M.void . CC.forkIO . sendAnnounceThread conf myThId $ mBox
             | isDuringWorkingTime t ->
                {-
                    立会時間中
                    スレッドに作業開始指示を送る
                -}
                M.mapM_ sendSigRun threads
            _ ->
                {-
                    立会時間外
                    スレッドに作業終了指示を送る
                -}
                M.mapM_ sendSigRunIsOver threads
        {-
            このスレッドは作業の担当スレッドに
            指示することしかしないので適当に時間をつぶす
        -}
        doSleep 1
        `onException`
            -- 子スレッドに作業終了指示を送っておく
            M.mapM_ sendSigRunIsOver threads
    {- |
        スレッドに作業開始指示を送る関数
        スレッドが終了していたなら起動する
    -}
    sendSigRun :: (MVar ThMsgSig, MVar ThMsgSig -> IO ()) -> IO ()
    sendSigRun (mBox, fnc) = do
        msg <- readMVar mBox
        case msg of
            {-
                スレッドが終了していたなら起動して
                作業開始指示を送る
            -}
            Donothing -> do
                sendMsgSig mBox Run
                CC.forkIO (fnc mBox)
                -- 起動タイミングをずらすために28.657秒待つ
                CC.threadDelay (28657 * 1000)
            {-
                作業開始指示または
                作業終了指示が送られている場合はそのままにする
            -}
            _ -> return ()

    {- |
        スレッドに作業終了指示を送る関数
    -}
    sendSigRunIsOver :: (MVar ThMsgSig, MVar ThMsgSig -> IO ()) -> IO ()
    sendSigRunIsOver (mBox, fnc) = do
        msg <- readMVar mBox
        case msg of
            -- スレッドが終了していたなら書き換えずにそのままにする
            Donothing -> return ()
            -- 作業終了指示を送る
            _ -> sendMsgSig mBox RunIsOver
    {- |
        全ての例外ハンドラ
    -}
    handleCatchAll :: Conf.Info -> SomeException -> IO ()
    handleCatchAll conf ex =
        let errmsg = T.pack . Printf.printf
                        "exception caught, \"%s\""
                        $ show ex
        in
        {-
            Slackへエラーメッセージを送る
        -}
        toSlack (Conf.slack conf) errmsg

{- |
    エントリポイント
-}
main :: IO ()
main =
    applicationBody "conf.json"

