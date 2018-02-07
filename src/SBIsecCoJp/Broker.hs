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
Module      :  SBIsecCoJp.Broker
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
{-# LANGUAGE TypeFamilies          #-}
module SBIsecCoJp.Broker
    ( siteConn
    , noticeOfBrokerageAnnouncement
    , noticeOfCurrentAssets
    , fetchUpdatedPriceAndStore
    ) where
import qualified Control.Concurrent           as CC
import           Control.Exception            (throwIO)
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Reader         as M
import           Control.Monad.Trans.Maybe    (MaybeT (..))
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
import qualified Safe

import qualified BrokerBackend                as BB
import qualified Conf
import qualified Lib
import           SBIsecCoJp.Model
import qualified SBIsecCoJp.Scraper           as S
import qualified ScraperBackend               as SB
import qualified SinkSlack                    as Slack

-- |
-- runResourceTと組み合わせて証券会社のサイトにログイン/ログアウトする
siteConn    :: (Monad m, M.MonadTrans t, MR.MonadResource (t m))
            => Conf.InfoSBIsecCoJp
            -> Conf.UserAgent
            -> (BB.HTTPSession -> m b)
            -> t m b
siteConn conf userAgent f =
    MR.allocate (login conf userAgent url) logout
    >>= (\(_,session) -> M.lift $ f session)
    where
    url = "https://k.sbisec.co.jp/bsite/visitor/top.do"

-- |
-- Slackへお知らせを送るついでに現在資産評価をDBへ
noticeOfBrokerageAnnouncement   :: M.MonadIO m
                                => Conf.InfoSBIsecCoJp
                                -> MySQL.ConnectInfo
                                -> Conf.UserAgent
                                -> C.Source m TL.Text
noticeOfBrokerageAnnouncement conf _ userAgent = do
    r <- M.liftIO
            . MR.runResourceT
            . siteConn conf userAgent $ \session -> do
        -- トップ -> マーケット情報を見に行く
        TL.pack . maybe "マーケット情報がありません。" show <$> runMaybeT (goMarketInfoPage session)
    C.yield r
    where
    -- |
    -- トップ -> マーケット情報を見に行く関数
    goMarketInfoPage :: BB.HTTPSession -> MaybeT IO S.MarketInfoPage
    goMarketInfoPage sess =
        MaybeT . return . S.marketInfoPage =<< fetchLinkPage sess "マーケット情報"

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
        DB.runMigration migrateSBIsecCoJp
        --
        yesterday <- takeBeforeAsset openingTime
        latest <- takeLatestAsset
        report <- M.mapM
                    (makeReport yesterday)
                    (latest :: Maybe (DB.Entity SbiseccojpAsset))
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
            { Slack.reportAt            = sbiseccojpAssetAt asset
            , Slack.reportAllAsset      = allAsset asset
            -- 現在値 - 前営業日値
            , Slack.reportGrowthToday   = (\y -> allAsset asset - allAsset y) <$> yesterday
            , Slack.reportAllProfit     = sbiseccojpAssetProfit asset
            , Slack.reportStockDigests  =
                [Slack.StockDigest (sbiseccojpStockGain s) (sbiseccojpStockDigest s) | s<-stocks]
            }
    -- |
    -- DBから最新の資産評価を取り出す
    takeLatestAsset =
        DB.selectFirst [] [DB.Desc SbiseccojpAssetAt]
    -- |
    -- DBから保有株式を取り出す
    takeStocks key =
        fmap DB.entityVal
        <$>
        DB.selectList
            [SbiseccojpStockAsset ==. key]
            [DB.Asc SbiseccojpStockTicker]
    -- |
    -- DBから前場開始直前の資産評価を取り出す
    takeBeforeAsset openingTime =
        fmap DB.entityVal
        <$>
        DB.selectFirst
            [SbiseccojpAssetAt <. openingTime]
            [DB.Desc SbiseccojpAssetAt]
    -- |
    -- 全財産（現金換算）を返す関数
    -- 株式資産評価合計 + 使用可能現金
    allAsset :: SbiseccojpAsset -> Double
    allAsset a =
        sbiseccojpAssetEvaluation a + realToFrac (sbiseccojpAssetCashBalance a)

-- |
-- 現在資産評価を証券会社のサイトから取得してDBへ
fetchUpdatedPriceAndStore   :: MySQL.ConnectInfo
                            -> BB.HTTPSession
                            -> IO ()
fetchUpdatedPriceAndStore connInfo sess@BB.HTTPSession{..} = do
    let mbfn a b = maybe (throwIO $ userError a) return =<< runMaybeT b
    -- トップ -> 買付余力を見に行く
    pmlPages <- mbfn "買付余力ページの取得に失敗しました"goPurchaseMarginListPage
    -- 先頭の買付余力しかいらないので
    let pmlist = S.PurchaseMarginListPage
                    . (\(S.PurchaseMarginListPage xs) -> take 1 xs) $ pmlPages
    -- トップ -> 買付余力 -> 詳細を見に行く関数
    pmdPages <- mbfn "買付余力 -> 詳細ページの取得に失敗しました" $ goPurchaseMarginDetailPage pmlist
    let pmdPage = Safe.headNote "買付余力/詳細の数が不足" pmdPages
    --
    -- サーバーに対して過度なアクセスを控えるための時間待ち
    CC.threadDelay (600 * 1000)
    --
    -- トップ -> 保有証券一覧を見に行く
    hslPage <- mbfn "保有証券一覧ページの取得に失敗しました" goHoldStockListPage
    -- トップ -> 保有証券一覧 -> 保有証券詳細ページを見に行く
    stocks <- mbfn "保有証券詳細ページの取得に失敗しました" $ goHoldStockDetailPage hslPage
    -- 受信時間
    tm <- Tm.getCurrentTime
    -- 全てをデーターベースへ
    ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateSBIsecCoJp
        -- 資産テーブルへ格納する
        key <- DB.insert $ asset tm hslPage pmdPage
        -- 保有株式テーブルへ格納する
        M.mapM_ (DB.insert . stock key) stocks
    where
    --
    --
    asset :: Tm.UTCTime -> S.HoldStockListPage -> S.PurchaseMarginDetailPage -> SbiseccojpAsset
    asset at hslp pmdp = SbiseccojpAsset
        { sbiseccojpAssetAt         = at
        , sbiseccojpAssetEvaluation = S.hslEvaluation hslp
        , sbiseccojpAssetProfit     = S.hslProfit hslp
        --
        , sbiseccojpAssetMoneySpare = S.pmdMoneyToSpare pmdp
        , sbiseccojpAssetCashBalance= S.pmdCashBalance pmdp
        }
    --
    --
    stock :: DB.Key SbiseccojpAsset -> S.HoldStockDetailPage -> SbiseccojpStock
    stock key S.HoldStockDetailPage{..} = SbiseccojpStock
        { sbiseccojpStockAsset      = key
        , sbiseccojpStockTicker     = hsdTicker
        , sbiseccojpStockCaption    = T.unpack hsdCaption
        , sbiseccojpStockCount      = hsdCount
        , sbiseccojpStockPurchase   = hsdPurchasePrice
        , sbiseccojpStockPrice      = hsdPrice
        }
    -- |
    -- トップ / 保有証券一覧を見に行く関数
    goHoldStockListPage :: MaybeT IO S.HoldStockListPage
    goHoldStockListPage =
        MaybeT . return . S.holdStockListPage =<< fetchLinkPage sess "保有証券一覧"
    -- |
    -- トップ -> 保有証券一覧 -> 保有証券詳細ページを見に行く関数
    goHoldStockDetailPage   :: S.HoldStockListPage
                            -> MaybeT IO [S.HoldStockDetailPage]
    goHoldStockDetailPage page =
        -- 詳細ページをスクレイピングする
        MaybeT . return . M.mapM S.holdStockDetailPage
        -- 詳細ページへアクセスする
        =<< M.mapM (fetch sess) . mapAbsUri . unpack =<< pure page
        where
        unpack p =
            let (S.HoldStockDetailLink xs) = S.hslLinks p in
            xs
    -- |
    -- トップ -> 買付余力を見に行く関数
    goPurchaseMarginListPage :: MaybeT IO S.PurchaseMarginListPage
    goPurchaseMarginListPage =
        S.purchaseMarginListPage <$> fetchLinkPage sess "買付余力"
    -- |
    -- トップ -> 買付余力 -> 詳細を見に行く関数
    goPurchaseMarginDetailPage  :: S.PurchaseMarginListPage
                                -> MaybeT IO [S.PurchaseMarginDetailPage]
    goPurchaseMarginDetailPage page =
        -- 詳細ページをスクレイピングする
        MaybeT . return . M.mapM S.purchaseMarginDetailPage
        -- 詳細ページへアクセスする
        =<< M.mapM (fetch sess) . mapAbsUri . unpack =<< pure page
        where
        unpack (S.PurchaseMarginListPage x) = x
    -- |
    -- リンク情報からURIに写す
    mapAbsUri :: [S.TextAndHref] -> [N.URI]
    mapAbsUri =
        Mb.mapMaybe (BB.toAbsoluteURI sLoginPageURI . T.unpack . snd)

-- |
-- リンクのページへアクセスする関数
fetchLinkPage :: BB.HTTPSession -> T.Text -> MaybeT IO TL.Text
fetchLinkPage sess t =
    fetch sess =<< MaybeT . return . lookupLinkOnTopPage sess =<< pure t

-- |
-- 引数のページへアクセスしてページを返す
fetch :: M.MonadIO m => BB.HTTPSession -> N.URI -> m TL.Text
fetch BB.HTTPSession{..} =
    fmap BB.takeBodyFromResponse
    . BB.fetchPage sManager sReqHeaders (Just sRespCookies) []

-- |
-- トップページ上のリンクテキストに対応したURIを返す
lookupLinkOnTopPage :: BB.HTTPSession -> T.Text -> Maybe N.URI
lookupLinkOnTopPage BB.HTTPSession{..} linktext =
    BB.toAbsoluteURI sLoginPageURI . T.unpack
    =<< lookup linktext (S.getTopPage $ S.topPage sTopPageHTML)

-- |
-- ログインページからログインしてHTTPセッション情報を返す関数
login :: Conf.InfoSBIsecCoJp -> Conf.UserAgent -> String -> IO BB.HTTPSession
login (Conf.InfoSBIsecCoJp conf) userAgent loginPageURL = do
    loginURI <- maybe errInvalidUrl return (N.parseURI loginPageURL)
    -- HTTPS接続ですよ
    manager <- N.newManager N.tlsManagerSettings
    -- ログインページへアクセスする
    loginPage <- BB.takeBodyFromResponse <$>
                    BB.fetchPage manager reqHeader Nothing [] loginURI
    -- ログインページをスクレイピングする
    loginForm <- either BB.failureAtScraping return $ S.formLoginPage loginPage
    -- IDとパスワードを入力する
    let postMsg = BB.mkCustomPostReq
                    (map SB.toPairNV $ SB.formInputTag loginForm)
                    [ ("username", Conf.loginID conf)
                    , ("password", Conf.loginPassword conf)
                    ]
    -- フォームのaction属性ページ
    let formAction = T.unpack $ SB.formAction loginForm
    postto <- maybe loginFail return $ BB.toAbsoluteURI loginURI formAction
    -- 提出
    resp <- BB.fetchPage manager reqHeader Nothing postMsg postto
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
        throwIO $ userError (loginPageURL ++ " は有効なURLではありません")
    --
    --
    loginFail =
       throwIO $ userError (loginPageURL ++ " にログインできませんでした")

-- |
-- ログアウトする関数
logout :: BB.HTTPSession -> IO ()
logout BB.HTTPSession{..} =
    let
        (S.TopPage xs) = S.topPage sTopPageHTML
    in do
    uri <- maybe logoutFail return $
            BB.toAbsoluteURI sLoginPageURI . T.unpack =<< lookup "ログアウト" xs
    -- ログアウトページへアクセスする
    M.void $ BB.fetchPage sManager sReqHeaders (Just sRespCookies) [] uri
    where
    --
    --
    logoutFail =
       throwIO $ userError "ログアウトリンクがわかりませんでした"


