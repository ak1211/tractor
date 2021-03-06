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
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module SBIsecCoJp.Broker
    ( siteConn
    , noticeOfBrokerageAnnouncement
    , noticeOfCurrentAssets
    , fetchUpdatedPriceAndStore
    ) where
import           Control.Exception.Safe
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as Logger
import qualified Control.Monad.Reader         as M
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import qualified Data.Maybe                   as Maybe
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Data.Time                    (UTCTime, ZonedTime (..))
import qualified Data.Time                    as Time
import           Database.Persist             ((<.), (==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import           Network.URI                  (URI)
import qualified Network.URI                  as URI
import qualified Safe

import qualified BrokerBackend                as BB
import qualified Conf
import qualified GenScraper                   as GS
import           Lib                          (tzAsiaTokyo)
import qualified Lib
import           SBIsecCoJp.Model
import qualified SBIsecCoJp.Scraper           as S
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
                                -> C.ConduitT () TL.Text m ()
noticeOfBrokerageAnnouncement conf _ userAgent = do
    r <- M.liftIO . MR.runResourceT . siteConn conf userAgent $ go
    C.yield r
    where
    go :: BB.HTTPSession -> IO TL.Text
    go sess =
        TL.pack . maybe "マーケット情報の取得に失敗しました。" show
        <$> runMaybeT (goMarketInfoPage sess)
    -- |
    -- トップ -> マーケット情報を見に行く関数
    goMarketInfoPage :: BB.HTTPSession -> MaybeT IO S.MarketInfoPage
    goMarketInfoPage sess =
        MaybeT . return . S.marketInfoPage =<< fetchLinkPage noWaitFetch sess "マーケット情報"

-- |
-- DBから最新の資産評価を取り出してSlackへレポートを送る
noticeOfCurrentAssets   :: M.MonadIO m
                        => MySQL.ConnectInfo
                        -> C.ConduitT () Slack.Report m ()
noticeOfCurrentAssets connInfo = do
    -- 今日の前場開始時間
    openingTime <- todayOpeningTime <$> M.liftIO Time.getCurrentTime
    -- データーベースの内容からレポートを作る
    rpt <- M.liftIO
            . Logger.runNoLoggingT . MR.runResourceT
            . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateSBIsecCoJp
        --
        yesterday <- takeBeforeAsset openingTime
        latest <- takeLatestAsset
        report <- M.mapM
                    (makeReport yesterday)
                    (latest :: Maybe (DB.Entity SbiseccojpAsset))
        return $ Maybe.maybeToList (report :: Maybe Slack.Report)
    -- Slackへレポートを送る
    CL.sourceList rpt
    where
    -- |
    -- 今日の前場開始時間
    todayOpeningTime :: UTCTime -> UTCTime
    todayOpeningTime =
        Time.zonedTimeToUTC
        . (\(ZonedTime t z) -> ZonedTime
            (t { Time.localTimeOfDay = Time.TimeOfDay 9 00 00}) z)
        . Time.utcToZonedTime Lib.tzAsiaTokyo
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
                [Slack.StockDigest (sbiseccojpStockAt s) (sbiseccojpStockGain s) (sbiseccojpStockDigest s) | s<-stocks]
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
    let mbfn a b = maybe (throwString a) return =<< runMaybeT b
    -- トップ -> 買付余力を見に行く
    pmlPages <- mbfn "買付余力ページの取得に失敗しました"goPurchaseMarginListPage
    -- 先頭の買付余力しかいらないので
    let pmlist = S.PurchaseMarginListPage
                    . (\(S.PurchaseMarginListPage xs) -> take 1 xs) $ pmlPages
    -- トップ -> 買付余力 -> 詳細を見に行く関数
    pmdPages <- mbfn "買付余力 -> 詳細ページの取得に失敗しました" $ goPurchaseMarginDetailPage pmlist
    let pmdPage = Safe.headNote "買付余力/詳細の数が不足" pmdPages

    -- サーバーに対して過度なアクセスを控えるための時間待ち
    BB.waitMS 600

    -- トップ -> 保有証券一覧を見に行く
    hslPage <- mbfn "保有証券一覧ページの取得に失敗しました" goHoldStockListPage
    -- トップ -> 保有証券一覧 -> 保有証券詳細ページを見に行く
    stocks <- mbfn "保有証券詳細ページの取得に失敗しました" $ goHoldStockDetailPage hslPage
    -- 受信時間
    tm <- Time.getCurrentTime
    let (year,_,_) = Time.toGregorian (Time.utctDay tm)
    -- 全てをデーターベースへ
    Logger.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateSBIsecCoJp
        -- 資産テーブルへ格納する
        key <- DB.insert $ asset tm hslPage pmdPage
        -- 保有株式テーブルへ格納する(寄っている場合のみ)
        M.mapM_ DB.insert . Maybe.mapMaybe (stock year key) $ stocks
    where
    -- |
    -- 資産テーブル情報を組み立てる
    asset :: UTCTime -> S.HoldStockListPage -> S.PurchaseMarginDetailPage -> SbiseccojpAsset
    asset at hslp pmdp = SbiseccojpAsset
        { sbiseccojpAssetAt         = at
        , sbiseccojpAssetEvaluation = S.hslEvaluation hslp
        , sbiseccojpAssetProfit     = S.hslProfit hslp
        --
        , sbiseccojpAssetMoneySpare = S.pmdMoneyToSpare pmdp
        , sbiseccojpAssetCashBalance= S.pmdCashBalance pmdp
        }
    -- |
    -- 保有株式テーブル情報を組み立てる
    -- まだ寄っていない値を元に作らない
    stock :: Integer -> DB.Key SbiseccojpAsset -> S.HoldStockDetailPage -> Maybe SbiseccojpStock
    stock year key S.HoldStockDetailPage{..} = do
        (month, day, hour, minute) <- hsdMDHourMin
        d <- Time.fromGregorianValid year month day
        t <- Time.makeTimeOfDayValid hour minute 0
        let lt = Time.LocalTime {Time.localDay = d, Time.localTimeOfDay = t}
        Just SbiseccojpStock
            { sbiseccojpStockAsset      = key
            , sbiseccojpStockAt         = Time.localTimeToUTC tzAsiaTokyo lt
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
        MaybeT . return . S.holdStockListPage =<< fetchLinkPage slowlyFetch sess "保有証券一覧"
    -- |
    -- トップ -> 保有証券一覧 -> 保有証券詳細ページを見に行く関数
    goHoldStockDetailPage   :: S.HoldStockListPage
                            -> MaybeT IO [S.HoldStockDetailPage]
    goHoldStockDetailPage page =
        -- 詳細ページをスクレイピングする
        MaybeT . return . M.mapM S.holdStockDetailPage
        -- 詳細ページへアクセスする
        =<< M.mapM (slowlyFetch sess) . mapAbsUri . S.unHoldStockDetailLink . S.hslLinks
        =<< pure page
    -- |
    -- トップ -> 買付余力を見に行く関数
    goPurchaseMarginListPage :: MaybeT IO S.PurchaseMarginListPage
    goPurchaseMarginListPage =
        S.purchaseMarginListPage <$> fetchLinkPage slowlyFetch sess "買付余力"
    -- |
    -- トップ -> 買付余力 -> 詳細を見に行く関数
    goPurchaseMarginDetailPage  :: S.PurchaseMarginListPage
                                -> MaybeT IO [S.PurchaseMarginDetailPage]
    goPurchaseMarginDetailPage page =
        -- 詳細ページをスクレイピングする
        MaybeT . return . M.mapM S.purchaseMarginDetailPage
        -- 詳細ページへアクセスする
        =<< M.mapM (slowlyFetch sess) . mapAbsUri . unpack =<< pure page
        where
        unpack (S.PurchaseMarginListPage x) = x
    -- |
    -- リンク情報からURIに写す
    mapAbsUri :: [GS.AnchorTag] -> [URI]
    mapAbsUri =
        Maybe.mapMaybe (BB.toAbsoluteURI sLoginPageURI . T.unpack . GS.aHref)

-- |
-- リンクのページへアクセスする関数
fetchLinkPage   :: (BB.HTTPSession -> URI -> MaybeT IO TL.Text)
                -> BB.HTTPSession -> T.Text -> MaybeT IO TL.Text
fetchLinkPage fetcher sess t =
    fetcher sess =<< MaybeT . return . lookupLinkOnTopPage sess =<< pure t

-- |
-- トップページ上のリンクテキストに対応したURIを返す
lookupLinkOnTopPage :: BB.HTTPSession -> T.Text -> Maybe URI
lookupLinkOnTopPage BB.HTTPSession{..} linktext =
    BB.toAbsoluteURI sLoginPageURI . T.unpack
    =<< lookup linktext [(GS.aText a, GS.aHref a) | a<-S.unTopPage $ S.topPage sTopPageHTML]

-- |
-- 通常のfetch
noWaitFetch :: M.MonadIO m => BB.HTTPSession -> URI -> m TL.Text
noWaitFetch =
    BB.fetchPageWithSession

-- |
-- 時間待ち付きfetch
slowlyFetch :: M.MonadIO m => BB.HTTPSession -> URI -> m TL.Text
slowlyFetch x y = noWaitFetch x y <* M.liftIO (BB.waitMS 300)

-- |
-- ログインページからログインしてHTTPセッション情報を返す関数
login :: Conf.InfoSBIsecCoJp -> Conf.UserAgent -> String -> IO BB.HTTPSession
login conf userAgent loginPageURL = do
    loginURI <- maybe errInvalidUrl return (URI.parseURI loginPageURL)
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
                    [ ("username", Conf.loginID $ Conf.unInfoSBIsecCoJp conf)
                    , ("password", Conf.loginPassword $ Conf.unInfoSBIsecCoJp conf)
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
        logoutLink = lookup "ログアウト" [(GS.aText a, GS.aHref a) | a<-S.unTopPage topPageLinks]
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


