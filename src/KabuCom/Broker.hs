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
Module      :  KabuCom.Broker
Description :  broker
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

証券会社とやり取りするモジュールです。
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module KabuCom.Broker
    ( siteConn
    , noticeOfBrokerageAnnouncement
    , noticeOfCurrentAssets
    , fetchUpdatedPriceAndStore
    ) where
import qualified Control.Arrow                as A
import           Control.Exception.Safe
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import qualified Data.Maybe                   as Mb
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Time                    as Tm
import           Database.Persist             ((<.), (==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import qualified Network.URI                  as N

import qualified BrokerBackend                as BB
import qualified Conf
import qualified GenScraper                   as GS
import           KabuCom.Model
import qualified KabuCom.Scraper              as S
import           Lib                          (tzAsiaTokyo)
import qualified Lib
import           Scheduling                   (AsiaTokyoDay (..))
import qualified SinkSlack                    as Slack

-- |
-- runResourceTと組み合わせて証券会社のサイトにログイン/ログアウトする
siteConn    :: (Monad m, M.MonadTrans t, MR.MonadResource (t m))
            => Conf.InfoKabuCom
            -> Conf.UserAgent
            -> (BB.HTTPSession -> m b)
            -> t m b
siteConn conf userAgent f =
    MR.allocate (login conf userAgent url) logout
    >>= (\(_,session) -> M.lift $ f session)
    where
    url = "https://s10.kabu.co.jp/_mem_bin/light/login.asp?/light"

-- |
-- Slackへお知らせを送るついでに現在資産評価をDBへ
noticeOfBrokerageAnnouncement   :: M.MonadIO m
                                => Conf.InfoKabuCom
                                -> MySQL.ConnectInfo
                                -> Conf.UserAgent
                                -> C.Source m TL.Text
noticeOfBrokerageAnnouncement _ _ _=
    return ()

-- |
-- DBから最新の資産評価を取り出してSlackへレポートを送る
noticeOfCurrentAssets   :: M.MonadIO m
                        => MySQL.ConnectInfo
                        -> C.Source m Slack.Report
noticeOfCurrentAssets connInfo = do
    -- 今日の前場開始時間
    openingTime <- todayOpeningTime <$> M.liftIO Tm.getCurrentTime
    -- データーベースの内容からレポートを作る
    rpt <- M.liftIO
            . ML.runNoLoggingT . MR.runResourceT
            . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateKabuCom
        --
        yesterday <- takeBeforeAsset openingTime
        latest <- takeLatestAsset
        report <- M.mapM
                    (makeReport yesterday)
                    (latest :: Maybe (DB.Entity KabucomAsset))
        return $ Mb.maybeToList (report :: Maybe Slack.Report)
    -- Slackへレポートを送る
    CL.sourceList rpt
    where
    -- |
    -- 今日の前場開始時間
    todayOpeningTime :: Tm.UTCTime -> Tm.UTCTime
    todayOpeningTime =
        Tm.zonedTimeToUTC
        . (\(Tm.ZonedTime t z) -> Tm.ZonedTime
            (t { Tm.localTimeOfDay = Tm.TimeOfDay 9 00 00}) z)
        . Tm.utcToZonedTime Lib.tzAsiaTokyo
    -- |
    -- レポートを作る関数
    makeReport yesterday (DB.Entity key asset) = do
        -- 保有株式を取り出す
        stocks <- takeStocks key
        --
        return Slack.Report
            { Slack.reportAt            = kabucomAssetAt asset
            , Slack.reportAllAsset      = allAsset asset
            -- 現在値 - 前営業日値
            , Slack.reportGrowthToday   = (\y -> allAsset asset - allAsset y) <$> yesterday
            , Slack.reportAllProfit     = kabucomAssetProfit asset
            , Slack.reportStockDigests  =
                [Slack.StockDigest (kabucomStockAt s) (kabucomStockGain s) (kabucomStockDigest s) | s<-stocks]
            }
    -- |
    -- DBから最新の資産評価を取り出す
    takeLatestAsset =
        DB.selectFirst [] [DB.Desc KabucomAssetAt]
    -- |
    -- DBから保有株式を取り出す
    takeStocks key =
        fmap DB.entityVal
        <$>
        DB.selectList
            [KabucomStockAsset ==. key]
            [DB.Asc KabucomStockTicker]
    -- |
    -- DBから前場開始直前の資産評価を取り出す
    takeBeforeAsset openingTime =
        fmap DB.entityVal
        <$>
        DB.selectFirst
            [KabucomAssetAt <. openingTime]
            [DB.Desc KabucomAssetAt]
    -- |
    -- 全財産（現金換算）を返す関数
    -- 株式資産評価合計 + 使用可能現金
    allAsset :: KabucomAsset -> Double
    allAsset a =
        kabucomAssetEvaluation a + realToFrac (kabucomAssetCashBalance a)

-- |
-- 現在資産評価を証券会社のサイトから取得してDBへ
fetchUpdatedPriceAndStore   :: MySQL.ConnectInfo
                            -> BB.HTTPSession
                            -> IO ()
fetchUpdatedPriceAndStore connInfo sess@BB.HTTPSession{..} = do
    -- トップ -> 買付出金可能額 を見に行く
    pmPage <- goPurchaseMarginPage
    --
    -- サーバーに対して過度なアクセスを控えるための時間待ち
    BB.waitMS 600
    -- トップ -> 残高照会 を見に行く
    splPage <- goStockPositionListPage
    -- トップ -> 残高照会 -> 個別銘柄詳細ページを見に行く
    stocks <- goStockDetailPage splPage
    -- 受信時間
    tm <- Tm.getCurrentTime
    -- 全てをデーターベースへ
    ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateKabuCom
        -- 資産テーブルへ格納する
        key <- DB.insert $ asset tm splPage pmPage
        -- 保有株式テーブルへ格納する(寄っている場合のみ)
        M.mapM_ DB.insert . Mb.mapMaybe (stock key) $ stocks
    where
    -- |
    -- トップ -> 買付出金可能額 を見に行く
    goPurchaseMarginPage :: (M.MonadIO m, MonadThrow m) => m S.PurchaseMarginPage
    goPurchaseMarginPage =
        S.purchaseMarginPage =<< fetchLinkPage slowlyFetch sess "買付出金可能額"
    -- |
    -- トップ -> 残高照会 を見に行く関数
    goStockPositionListPage :: (M.MonadIO m, MonadThrow m) => m S.StockPositionListPage
    goStockPositionListPage =
        S.stockPositionListPage =<< fetchLinkPage slowlyFetch sess "残高照会"
    -- |
    -- トップ -> 残高照会 -> 個別銘柄詳細ページを見に行く関数
    goStockDetailPage   :: (M.MonadIO m, MonadThrow m)
                        => S.StockPositionListPage
                        -> m [(S.StockPositionItem, S.StockDetailPage)]
    goStockDetailPage page =
        let ps = S.splPositions page
        in
        M.mapM (\i -> (,) <$> pure i <*> go i) ps
        where
        go :: (M.MonadIO m, MonadThrow m) => S.StockPositionItem -> m S.StockDetailPage
        go spi =
            let href = T.unpack . GS.aHref $ S.spCaptionAnchor spi
            in
            case BB.toAbsoluteURI sLoginPageURI href of
                Nothing -> throwString $ href ++" の絶対リンクを取得できませんでした"
                Just uri ->
                    -- 詳細ページへアクセスしてスクレイピングする
                    S.stockDetailPage =<< slowlyFetch sess uri
    -- |
    -- 資産テーブル情報を組み立てる
    asset :: Tm.UTCTime -> S.StockPositionListPage -> S.PurchaseMarginPage -> KabucomAsset
    asset at splp pmp = KabucomAsset
        { kabucomAssetAt         = at
        , kabucomAssetEvaluation = S.splEvaluation splp
        , kabucomAssetProfit     = S.splProfit splp
        --
        , kabucomAssetMoneySpare = S.pmMoneyToSpare pmp
        , kabucomAssetCashBalance= S.pmCashBalance pmp
        }
    -- |
    -- 保有株式テーブル情報を組み立てる
    -- まだ寄っていない値を元に作らない
    stock   :: DB.Key KabucomAsset
            -> (S.StockPositionItem, S.StockDetailPage)
            -> Maybe KabucomStock
    stock key (sp, sdp) = do
        (pr, (h,m)) <- A.second S.getHourMinute <$> S.sdpPrice sdp
        t <- Tm.makeTimeOfDayValid h m 0
        let lt = Tm.LocalTime   { Tm.localDay = getAsiaTokyoDay (S.sdpDay sdp)
                                , Tm.localTimeOfDay = t }
        Just KabucomStock
            { kabucomStockAsset      = key
            , kabucomStockAt         = Tm.localTimeToUTC tzAsiaTokyo lt
            , kabucomStockTicker     = S.sdpTicker sdp
            , kabucomStockCaption    = T.unpack $ S.sdpCaption sdp
            , kabucomStockCount      = S.spCount sp
            , kabucomStockPurchase   = S.spPurchasePrice sp
            , kabucomStockPrice      = pr
            }

-- |
-- リンクのページへアクセスする関数
fetchLinkPage :: MonadThrow m => (BB.HTTPSession -> N.URI -> m TL.Text)
                -> BB.HTTPSession -> T.Text -> m TL.Text
fetchLinkPage fetcher sess t =
    fetcher sess =<< lookupLinkOnTopPage sess =<< pure t

-- |
-- トップページ上のリンクテキストに対応したURIを返す
lookupLinkOnTopPage :: MonadThrow m => BB.HTTPSession -> T.Text -> m N.URI
lookupLinkOnTopPage BB.HTTPSession{..} linktext =
    maybe failure pure go
    where
    --
    --
    go =
        BB.toAbsoluteURI sLoginPageURI . T.unpack
        =<< lookup linktext [(GS.aText a, GS.aHref a) | a<-topPage]
    --
    --
    topPage =
        S.getTopPage $ S.topPage sTopPageHTML
    --
    --
    failure =
        throwString $ "no link \"" ++ T.unpack linktext ++ "\""

-- |
-- 通常のfetch
noWaitFetch :: M.MonadIO m => BB.HTTPSession -> N.URI -> m TL.Text
noWaitFetch =
    BB.fetchPageWithSession

-- |
-- 時間待ち付きfetch
slowlyFetch :: M.MonadIO m => BB.HTTPSession -> N.URI -> m TL.Text
slowlyFetch x y = noWaitFetch x y <* M.liftIO (BB.waitMS 300)

-- |
-- ログインページからログインしてHTTPセッション情報を返す関数
login :: Conf.InfoKabuCom -> Conf.UserAgent -> String -> IO BB.HTTPSession
login conf userAgent loginPageURL = do
    loginURI <- maybe errInvalidUrl return (N.parseURI loginPageURL)
    -- HTTPS接続ですよ
    manager <- N.newManager N.tlsManagerSettings
    -- ログインページへアクセスする
    loginPage <- BB.takeBodyFromResponse <$>
                    BB.fetchHTTP manager reqHeader Nothing [] loginURI
    -- ログインページをスクレイピングする
    loginForm <- S.formLoginPage loginPage
    -- IDとパスワードを入力する
    let postMsg = BB.mkCustomPostReq
                    (map GS.toPairNV $ GS.formInputTag loginForm)
                    [ ("SsLogonUser", Conf.loginID $ Conf.getInfoKabuCom conf)
                    , ("SsLogonPassword", Conf.loginPassword $ Conf.getInfoKabuCom conf)
                    ]
    -- フォームのaction属性ページ
    let formAction = T.unpack $ GS.formAction loginForm
    postto <- maybe loginFail return $ BB.toAbsoluteURI loginURI formAction
    -- 提出
    resp <- BB.fetchHTTP manager reqHeader Nothing postMsg postto
    -- 受け取ったセッションクッキーとトップページを返却する
    return BB.HTTPSession
        { BB.sLoginPageURI = loginURI
        , BB.sManager      = manager
        , BB.sReqHeaders   = reqHeader
        , BB.sRespCookies  = N.responseCookieJar resp
        , BB.sTopPageHTML  = BB.takeBodyFromResponse resp
        }
    where
    -- |
    -- HTTPリクエストヘッダ
    reqHeader = Lib.httpRequestHeader userAgent
    --
    --
    errInvalidUrl =
        throwString $ loginPageURL ++ " は有効なURLではありません"
    --
    --
    loginFail =
       throwString $ loginPageURL ++ " にログインできませんでした"


-- |
-- ログアウトする関数
logout :: BB.HTTPSession -> IO ()
logout sess@BB.HTTPSession{..} =
    let topPageLinks = S.topPage sTopPageHTML
        logoutLink = lookup "LOG OUT" [(GS.aText a, GS.aHref a) | a<-S.getTopPage topPageLinks]
        toLogoutURI = BB.toAbsoluteURI sLoginPageURI . T.unpack
    in
    case toLogoutURI =<< logoutLink of
    Nothing -> logoutFail
    Just uri ->
        -- ログアウトページへアクセスする
        M.void $ BB.fetchPageWithSession sess uri
    where
    --
    --
    logoutFail =
       throwString "ログアウトリンクがわかりませんでした"

