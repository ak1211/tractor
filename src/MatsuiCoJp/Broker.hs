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
Module      :  MatsuiCoJp.Broker
Description :  broker
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

証券会社とやり取りするモジュールです。
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module MatsuiCoJp.Broker
    ( siteConn
    , noticeOfBrokerageAnnouncement
    , noticeOfCurrentAssets
    , fetchUpdatedPriceAndStore
    , SellOrder (..)
    , sellStock
    ) where

import qualified Control.Arrow                as A
import qualified Control.Concurrent           as CC
import           Control.Exception            (throwIO)
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.Logger         as ML
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.Text.Lazy               as TL
import qualified Data.Time                    as Tm
import           Database.Persist             ((<.), (==.))
import qualified Database.Persist             as DB
import qualified Database.Persist.MySQL       as MySQL
import qualified Database.Persist.Sql         as DB
import qualified Network.HTTP.Conduit         as N
import qualified Network.HTTP.Types.Header    as N
import qualified Network.URI                  as N
import qualified Text.HTML.TagSoup            as TS
import qualified Text.HTML.TagSoup.Tree       as TS

import qualified BrokerBackend                as BB
import qualified Conf
import qualified Lib
import           MatsuiCoJp.Model
import qualified MatsuiCoJp.Scraper
import qualified SinkSlack                    as Slack

-- |
-- runResourceTと組み合わせて証券会社のサイトにログイン/ログアウトする
siteConn    :: (Monad m, M.MonadTrans t, MR.MonadResource (t m))
            => Conf.InfoMatsuiCoJp
            -> (BB.HTTPSession -> m b)
            -> t m b
siteConn conf f =
    MR.allocate login' logout
    >>= (\(_,session) -> M.lift $ f session)
    where
    --
    --
    url = Conf.loginURL conf
    --
    --
    invalidUrl = throwIO . userError $ url ++ " は有効なURLではありません"
    --
    --
    loginFail = throwIO . userError $ url ++ " にログインできませんでした"
    -- |
    -- 証券会社のサイトにログインする関数
    login' =
        maybe invalidUrl return (N.parseURI url)
        >>= MatsuiCoJp.Broker.login conf
        >>= maybe loginFail return

-- |
-- Slackへお知らせを送るついでに現在資産評価をDBへ
noticeOfBrokerageAnnouncement   :: M.MonadIO m
                                => MySQL.ConnectInfo
                                -> Conf.InfoMatsuiCoJp
                                -> C.Source m TL.Text
noticeOfBrokerageAnnouncement connInfo conf = do
    r <- M.liftIO
            . MR.runResourceT
            . siteConn conf $ \session -> do
        -- ホーム -> お知らせを見に行く
        fha <- MatsuiCoJp.Broker.fetchFraHomeAnnounce session
        -- 現在資産評価を証券会社のサイトから取得してDBへ
        fetchUpdatedPriceAndStore connInfo session
        return (text fha)
    -- Slackへお知らせを送る
    C.yield r
    where
    text v =
        TL.unlines $
            [ MatsuiCoJp.Scraper.fsAnnounceDeriverTime v
            , MatsuiCoJp.Scraper.fsAnnounceLastLoginTime v ]
            ++ MatsuiCoJp.Scraper.fsAnnounces v

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
        DB.runMigration migrateMatsuiCoJp
        --
        yesterday <- takeBeforeAsset openingTime
        latest <- takeLatestAsset
        report <- M.mapM
                    (makeReport yesterday)
                    (latest :: Maybe (DB.Entity MatsuicojpAsset))
        return $ Maybe.maybeToList (report :: Maybe Slack.Report)
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
        . Tm.utcToZonedTime Lib.tzJST
    -- |
    -- レポートを作る関数
    makeReport yesterday (DB.Entity key asset) = do
        -- 保有株式を取り出す
        stocks <- takeStocks key
        --
        return Slack.Report
            { Slack.reportAt            = matsuicojpAssetAt asset
            , Slack.reportAllAsset      = allAsset asset
            -- 現在値 - 前営業日値
            , Slack.reportGrowthToday   = (\y -> allAsset asset - allAsset y) <$> yesterday
            , Slack.reportAllProfit     = matsuicojpAssetProfit asset
            , Slack.reportStockDigests  =
                [Slack.StockDigest (matsuicojpStockGain s) (matsuicojpStockDigest s) | s<-stocks]
            }
    -- |
    -- DBから最新の資産評価を取り出す
    takeLatestAsset =
        DB.selectFirst [] [DB.Desc MatsuicojpAssetAt]
    -- |
    -- DBから保有株式を取り出す
    takeStocks key =
        DB.selectList
            [MatsuicojpStockAsset ==. key]
            [DB.Asc MatsuicojpStockTicker]
        >>= return . map DB.entityVal
    -- |
    -- DBから前場開始直前の資産評価を取り出す
    takeBeforeAsset openingTime =
        DB.selectFirst
            [MatsuicojpAssetAt <. openingTime]
            [DB.Desc MatsuicojpAssetAt]
        >>= return . fmap DB.entityVal
    -- |
    -- 全財産（現金換算）を返す関数
    -- 株式資産評価合計 + 使用可能現金
    --
    -- 以下の手数料関係は夕方バッチ処理で決まるので算入しない
    -- 預り増加額
    -- 預り減少額
    -- ボックスレート手数料拘束金
    -- 源泉徴収税拘束金（仮計算）
    allAsset :: MatsuicojpAsset -> Double
    allAsset a =
        matsuicojpAssetEvaluation a + realToFrac (matsuicojpAssetCash a)

-- |
-- 現在資産評価を証券会社のサイトから取得してDBへ
fetchUpdatedPriceAndStore   :: MySQL.ConnectInfo
                            -> BB.HTTPSession
                            -> IO ()
fetchUpdatedPriceAndStore connInfo session = do
    -- 株式取引 -> 現物売を見に行く
    sell <- MatsuiCoJp.Broker.fetchFraStkSell session
    -- 資産状況 -> 余力情報を見に行く
    assetspare <- MatsuiCoJp.Broker.fetchFraAstSpare session
    -- 受信時間
    tm <- Tm.getCurrentTime
    -- 全てをデーターベースへ
    ML.runStderrLoggingT . MR.runResourceT . MySQL.withMySQLConn connInfo . MySQL.runSqlConn $ do
        DB.runMigration migrateMatsuiCoJp
        -- 資産テーブルへ格納する
        key <- DB.insert $ asset tm sell assetspare
        -- 保有株式テーブルへ格納する
        M.mapM_ (DB.insert . stock key) $ MatsuiCoJp.Scraper.fsStocks sell
    where
    --
    --
    asset   :: Tm.UTCTime
            -> MatsuiCoJp.Scraper.FraStkSell
            -> MatsuiCoJp.Scraper.FraAstSpare
            -> MatsuicojpAsset
    asset at fsell fas = MatsuicojpAsset
        { matsuicojpAssetAt             = at
        , matsuicojpAssetEvaluation     = MatsuiCoJp.Scraper.fsQuantity fsell
        , matsuicojpAssetProfit         = MatsuiCoJp.Scraper.fsProfit fsell
        --
        , matsuicojpAssetMoneySpare     = MatsuiCoJp.Scraper.faMoneyToSpare fas
        , matsuicojpAssetCashBalance    = MatsuiCoJp.Scraper.faStockOfMoney fas
        , matsuicojpAssetDepositInc     = MatsuiCoJp.Scraper.faIncreaseOfDeposits fas
        , matsuicojpAssetDepositDec     = MatsuiCoJp.Scraper.faDecreaseOfDeposits fas
        , matsuicojpAssetBindingFee     = MatsuiCoJp.Scraper.faRestraintFee fas
        , matsuicojpAssetBindingTax     = MatsuiCoJp.Scraper.faRestraintTax fas
        , matsuicojpAssetCash           = MatsuiCoJp.Scraper.faCash fas
        }
    --
    --
    stock   :: DB.Key MatsuicojpAsset
            -> MatsuiCoJp.Scraper.HoldStock
            -> MatsuicojpStock
    stock key hs = MatsuicojpStock
        { matsuicojpStockAsset      = key
        , matsuicojpStockTicker     = TSTYO $ fromIntegral(MatsuiCoJp.Scraper.hsCode hs)
        , matsuicojpStockCaption    = TL.unpack (MatsuiCoJp.Scraper.hsCaption hs)
        , matsuicojpStockCount      = MatsuiCoJp.Scraper.hsCount hs
        , matsuicojpStockPurchase   = MatsuiCoJp.Scraper.hsPurchasePrice hs
        , matsuicojpStockPrice      = MatsuiCoJp.Scraper.hsPrice hs
        }

-- |
-- formのactionを実行する関数
doPostAction    :: N.Manager
                -> N.RequestHeaders
                -> Maybe N.CookieJar
                -> [(B8.ByteString, B8.ByteString)]
                -> N.URI
                -> TL.Text
                -> IO (Maybe (N.Response BL8.ByteString))
doPostAction manager reqHeader cookie customPostReq pageURI html = do
    let uTree = TS.universeTree $ TS.tagTree $ TS.parseTags html
    let formTags = [formTag | formTag@(TS.TagBranch nm _ _) <- uTree, "form"==TL.toLower nm]
    case formTags of
        -- ページ唯一のformタグを取り出す
        x : _ -> action x
        _     -> return Nothing
    where
    --
    --
    action :: TS.TagTree TL.Text -> IO (Maybe (N.Response BL8.ByteString))
    action = \case
        form@(TS.TagBranch _  attrs _) -> do
            let defaultPostReqBody = defaultRequest form
            -- formタグの属性を取り出す
            let fmAction = Maybe.listToMaybe [v | (k,v)<-attrs, "action"==TL.toLower k]
            let postReqBody = Maybe.catMaybes
                                $ uncurry (List.zipWith chooseDefaultOrCustomReq)
                                $ List.unzip defaultPostReqBody
            -- POSTリクエストを送信するURL
            let postActionURL = BB.toAbsoluteURI pageURI =<< fmAction
            -- フォームのaction属性ページへアクセス
            M.mapM (BB.fetchPage manager reqHeader cookie postReqBody) (postActionURL :: Maybe N.URI)
        _ -> return Nothing
    --
    --
    chooseDefaultOrCustomReq :: B8.ByteString -> B8.ByteString -> Maybe (B8.ByteString, B8.ByteString)
    chooseDefaultOrCustomReq k v =
        case List.lookup k customPostReq of
            Just cv -> Just (k, cv)
            -- x yは指定の物以外削除
            Nothing | B8.isSuffixOf ".x" k -> Nothing
            Nothing | B8.isSuffixOf ".y" k -> Nothing
            _       ->  Just (k, v)
    --
    --
    takeValue :: [TS.Attribute TL.Text] -> TL.Text -> Maybe B8.ByteString
    takeValue attrs name =
        B8.pack . TL.unpack . snd <$> List.find ((==) (TL.toCaseFold name) . TL.toCaseFold . fst) attrs
    -- |
    -- 未入力時のフォームリクエストを得る
    defaultRequest :: TS.TagTree TL.Text -> [(B8.ByteString, B8.ByteString)]
    defaultRequest (TS.TagLeaf _) = undefined
    defaultRequest (TS.TagBranch _  _ childNodes) =
        let sel = [selectTag br | br@(TS.TagBranch nm _ _) <- TS.universeTree childNodes, "select"==TL.toLower nm] in
        let ias = [as | TS.TagOpen nm as <- TS.flattenTree childNodes, "input"==TL.toLower nm] in
        let inp = List.concatMap inputTag ias in
        let img = List.concatMap inputTypeImage ias in
        Maybe.catMaybes sel ++ inp ++ img
        where
        --
        --
        selectTag :: TS.TagTree TL.Text -> Maybe (B8.ByteString, B8.ByteString)
        selectTag (TS.TagLeaf _) = undefined
        selectTag (TS.TagBranch _ attrs' children) =
            -- タグ内のoptionタグを全て取り出す
            let options = [as | TS.TagOpen nm as <- TS.flattenTree children, "option"==TL.toLower nm] in
            List.find (\as -> Maybe.isJust $ takeValue as "selected") options
            >>= \as -> (\k v -> (k,v)) <$> takeValue attrs' "name" <*> takeValue as "value"
        --
        --
        inputTag :: [TS.Attribute TL.Text] -> [(B8.ByteString, B8.ByteString)]
        inputTag attrs' =
            let f = takeValue attrs' in
            -- (name, value)のタプルを作る
            case f "type" of
                Just "checkbox" | Maybe.isNothing (f "checked") -> []
                Just "radio" | Maybe.isNothing (f "checked") -> []
                _ -> Maybe.maybeToList $ (\a b -> (a,b)) <$> f "name" <*> f "value"
        --
        -- input type="image"
        inputTypeImage :: [TS.Attribute TL.Text] -> [(B8.ByteString, B8.ByteString)]
        inputTypeImage attrs' =
            let f = takeValue attrs' in
            let name = f "name" in
            -- (name, value)のタプルを作る
            case f "type" of
                Just "image" | Maybe.isJust name ->
                    let nm = Maybe.fromJust name in
                    [(nm `B8.append` ".x", "0"), (nm `B8.append` ".y", "0")]
                _ -> []

-- |
-- HTTPセッション中にformのactionを実行する関数
doPostActionOnSession   :: BB.HTTPSession
                        -> [(B8.ByteString, B8.ByteString)]
                        -> TL.Text
                        -> IO (Maybe (N.Response BL8.ByteString))
doPostActionOnSession s customPostReq =
    doPostAction (BB.sManager s) (BB.sReqHeaders s) (Just $ BB.sRespCookies s) customPostReq (BB.sLoginPageURI s)

-- |
-- ログインページからログインしてHTTPセッション情報を返す関数
login :: Conf.InfoMatsuiCoJp -> N.URI -> IO (Maybe BB.HTTPSession)
login conf loginUri = do
    -- HTTPS接続ですよ
    manager <- N.newManager N.tlsManagerSettings
    -- ログインページへアクセス
    loginPageBody <- BB.takeBodyFromResponse
                        <$> BB.fetchPage manager reqHeader Nothing [] loginUri
    -- ログインページでID&パスワードをsubmit
    resp <- doPostAction manager reqHeader Nothing customPostReq loginUri loginPageBody
    let session = mkSession manager <$> resp
    return session
    where
    -- |
    -- HTTPリクエストヘッダ
    reqHeader =
        Lib.httpRequestHeader $ Conf.userAgent (conf::Conf.InfoMatsuiCoJp)
    -- |
    -- ログインID&パスワード
    customPostReq =
        [ ("clientCD", B8.pack $ Conf.loginID conf)
        , ("passwd", B8.pack $ Conf.loginPassword conf)
        ]
    -- |
    -- 返値組み立て
    mkSession manager resp =
        BB.HTTPSession
            { BB.sLoginPageURI = loginUri
            , BB.sManager      = manager
            , BB.sReqHeaders   = reqHeader
            , BB.sRespCookies  = N.responseCookieJar resp
            , BB.sTopPageHTML  = BB.takeBodyFromResponse resp
            }

-- |
-- リンクをクリックする関数
clickLinkText :: BB.HTTPSession -> TL.Text -> [TL.Text] -> IO [TL.Text]
clickLinkText session linkText htmls =
    M.mapM fetch linkPaths
    where
    --
    linkPaths = Maybe.mapMaybe (lookup linkText . getPageCaptionAndLink) htmls
    --
    fetch relativePath =
        BB.takeBodyFromResponse <$> BB.fetchInRelativePath session relativePath

-- |
-- GM / LMページからリンクテキストとリンクのタプルを取り出す関数
getPageCaptionAndLink :: TL.Text -> [(TL.Text, TL.Text)]
getPageCaptionAndLink html =
    let utree = TS.universeTree $ TS.tagTree $ TS.parseTags html in
    [(as,cs) | TS.TagBranch n as cs <- utree, "a"==TL.toLower n]
    >>= \(as,cs) -> List.zip
        -- リンクテキストを取り出す
        [t | TS.TagText t <- TS.flattenTree cs]
        -- リンクを取り出す
        [v | (k, v) <- as, "href"==TL.toLower k]

-- |
-- targetのフレームを処理する
dispatchFrameSet    :: BB.HTTPSession
                    -> [TL.Text]
                    -> (TL.Text, [TL.Text] -> IO [TL.Text])
                    -> IO [TL.Text]
dispatchFrameSet session htmls (targetFrmName, action) =
    case htmls of
        [] -> return []
        h : hs -> do
            -- frameタグから属性の(name, src)タプルを作る
            let frames = [as | TS.TagOpen nm as <- TS.parseTags h, "frame" == TL.toLower nm]
                        >>= Maybe.maybeToList . buildNameSrc
            -- targetのフレームを処理する
            case List.lookup targetFrmName frames of
                Nothing -> return []
                Just linkPath -> do
                    html <- BB.takeBodyFromResponse <$> BB.fetchInRelativePath session linkPath
                    r <- action [html]
                    s <- dispatchFrameSet session hs (targetFrmName, action)
                    return (r ++ s)
    where
    -- |
    -- frameタグから属性の(name, src)タプルを作る
    buildNameSrc :: [TS.Attribute TL.Text] -> Maybe (TL.Text, TL.Text)
    buildNameSrc attrList = do
        name <- Maybe.listToMaybe [v | (k, v) <- attrList, "name"==TL.toLower k]
        src  <- Maybe.listToMaybe [v | (k, v) <- attrList, "src"==TL.toLower k]
        Just (name, src)

type Scraper a      = ([TL.Text] -> Either TL.Text a)
type ClickAction    = (BB.HTTPSession -> [TL.Text] -> IO [TL.Text])
type PathOfContents = [(TL.Text, ClickAction)]

data ScrapingSet a  = ScrapingSet (Scraper a) PathOfContents

-- |
-- "ログアウト" のページ
setLogout :: ScrapingSet TL.Text
setLogout =
    ScrapingSet
        MatsuiCoJp.Scraper.nonScraping
        [ ("GM", flip clickLinkText "■ログアウト"), ("CT", const return) ]

-- |
-- "お知らせ"
-- "ホーム" -> "お知らせ" のページ
setFraHomeAnnounce :: ScrapingSet MatsuiCoJp.Scraper.FraHomeAnnounce
setFraHomeAnnounce =
    ScrapingSet
        MatsuiCoJp.Scraper.scrapingFraHomeAnnounce
        [ ("GM", flip clickLinkText "ホーム") , ("LM", flip clickLinkText "お知らせ") , ("CT", const return) ]

-- |
-- 現在の保有株情報
-- "株式取引" -> "現物売" のページ
setFraStkSell :: ScrapingSet MatsuiCoJp.Scraper.FraStkSell
setFraStkSell =
    ScrapingSet
        MatsuiCoJp.Scraper.scrapingFraStkSell
        [ ("GM", flip clickLinkText "株式取引"), ("LM", flip clickLinkText "現物売"), ("CT", const return) ]

-- |
-- 現在の資産情報
-- "資産状況" -> "余力情報" のページ
setFraAstSpare :: ScrapingSet MatsuiCoJp.Scraper.FraAstSpare
setFraAstSpare =
    ScrapingSet
        MatsuiCoJp.Scraper.scrapingFraAstSpare
        [ ("GM", flip clickLinkText "資産状況"), ("LM", flip clickLinkText "余力情報"), ("CT", const return) ]

-- |
-- 売り注文を出す関数
setSellStock :: SellOrder -> ScrapingSet MatsuiCoJp.Scraper.OrderConfirmed
setSellStock order =
    ScrapingSet
        MatsuiCoJp.Scraper.scrapingOrderConfirmed
        [ ("GM", flip clickLinkText "株式取引"), ("LM", flip clickLinkText "現物売"), ("CT", doSellOrder order) ]

-- |
-- サイトから目的のページをスクレイピングする関数
fetchSomethingPage  :: BB.HTTPSession
                    -> ScrapingSet a
                    -> IO a
fetchSomethingPage session (ScrapingSet scraper pathes) =
    either BB.failureAtScraping return =<< scraper <$> fetchTargetPage
    where
    -- |
    -- フレームのパスに従って目的のページを取得する関数
    fetchTargetPage = M.foldM dispatcher seed specificPaths
    --
    --
    dispatcher = dispatchFrameSet session
    seed = [BB.sTopPageHTML session]
    specificPaths = map (A.second ($ session)) pathes

-- |
-- ログアウトする関数
logout :: BB.HTTPSession -> IO ()
logout = M.void . flip fetchSomethingPage setLogout

-- |
-- "お知らせ"を得る
fetchFraHomeAnnounce :: BB.HTTPSession -> IO MatsuiCoJp.Scraper.FraHomeAnnounce
fetchFraHomeAnnounce = flip fetchSomethingPage setFraHomeAnnounce

-- |
-- 現在の保有株情報を得る
fetchFraStkSell :: BB.HTTPSession -> IO MatsuiCoJp.Scraper.FraStkSell
fetchFraStkSell = flip fetchSomethingPage setFraStkSell

-- |
-- 現在の資産情報を得る
fetchFraAstSpare :: BB.HTTPSession -> IO MatsuiCoJp.Scraper.FraAstSpare
fetchFraAstSpare = flip fetchSomethingPage setFraAstSpare

-- |
-- 注文情報
data SellOrder = SellOrder
    { sellOrderPassword :: String
    , sellOrderCode     :: Int
    , sellOrderNominal  :: Int
    , sellOrderPrice    :: Double
    }

-- |
-- 売り注文を出す関数
sellStock :: BB.HTTPSession -> SellOrder -> IO MatsuiCoJp.Scraper.OrderConfirmed
sellStock session order =
    fetchSomethingPage session (setSellStock order)

--
--
doSellOrder :: SellOrder -> BB.HTTPSession -> [TL.Text] -> IO [TL.Text]
doSellOrder order session orderPage =
    clickSellOrderLink orderPage
    >>= slowly >>= submitSellOrderPage
    >>= slowly >>= submitConfirmPage
    where
    --
    --
    slowly x = CC.threadDelay (300 * 1000) >> return x
    -- |
    -- 売り注文ページの所有株式リストにある
    -- 売り対象銘柄の売り注文リンクをクリックする
    clickSellOrderLink :: [TL.Text] -> IO [TL.Text]
    clickSellOrderLink htmls =
        (\x -> return [BB.takeBodyFromResponse x])
        =<< BB.fetchInRelativePath session
        =<< takeSellOrderUrl
        =<< takeHoldStocks
        where
        --
        --
        matchCode c =
            sellOrderCode order == MatsuiCoJp.Scraper.hsCode c
        --
        -- 所有株の中からcodeで指定された銘柄の売り注文ページリンクを取り出す
        takeSellOrderUrl:: [MatsuiCoJp.Scraper.HoldStock] -> IO TL.Text
        takeSellOrderUrl stocks =
            case List.find matchCode stocks of
                Nothing -> M.liftIO $ throwIO donothaveStockSellEx
                Just v ->
                    case MatsuiCoJp.Scraper.hsSellOrderUrl v of
                        Nothing -> M.liftIO $ throwIO cannotgotoSellPageEx
                        Just y -> return y
        --
        -- 売り注文ページから所有株式リストを取り出す
        takeHoldStocks :: IO [MatsuiCoJp.Scraper.HoldStock]
        takeHoldStocks =
            case MatsuiCoJp.Scraper.scrapingFraStkSell htmls of
                Left l -> M.liftIO . throwIO $ BB.UnexpectedHTMLException l
                Right r -> return (MatsuiCoJp.Scraper.fsStocks r)
    -- |
    -- 売り注文ページに注文を入力して送信する
    submitSellOrderPage :: [TL.Text] -> IO [TL.Text]
    submitSellOrderPage pages =
        either (M.liftIO . throwIO) return =<< go pages
        where
        go [] = return (Left cannotgetSellOrderPageEx)
        --
        -- 売り注文ページには次のページが無いので先頭のみを取り出す
        go (firstPage:_) =
            maybe   (Left cannotgotoConfirmPageEx)
                    (Right . replicate 1 . BB.takeBodyFromResponse)
            <$>
            -- 売り注文ページのフォームを提出する
            doPostActionOnSession session customPostReq firstPage
        --
        -- 売り注文ページのPOSTリクエストを組み立てる
        -- 以下は初期値のまま
        -- name="orderNari" 成行チェックボックス
        -- name="execCondCD" 執行条件ラジオボタン
        -- name="validDt" 有効期間ラジオボタン
        customPostReq =
            [ ("orderNominal", B8.pack . show $ sellOrderNominal order) -- 株数
            , ("orderPrc", B8.pack . show $ sellOrderPrice order)       -- 値段
            , ("tyukakuButton.x", "57")                                 -- 注文確認ボタンのクリック位置
            , ("tyukakuButton.y", "10")                                 -- 注文確認ボタンのクリック位置
            ]
    -- |
    -- 注文確認ページに取引暗証番号を入力して送信する
    submitConfirmPage :: [TL.Text] -> IO [TL.Text]
    submitConfirmPage pages =
        either (M.liftIO . throwIO) return =<< go pages
        where
        go [] = return (Left cannotgetConfirmPageEx)
        --
        -- 注文確認ページには次のページが無いので１ページ目のみが対象
        go (firstPage:_) =
            maybe   (Left cannotgotoAcceptedPageEx)
                    (Right . replicate 1 . BB.takeBodyFromResponse)
            <$>
            doPostActionOnSession session customPostReq firstPage
        --
        -- 注文確認ページのPOSTリクエスト
        customPostReq =
            [("pinNo", B8.pack $ sellOrderPassword order)]  -- 取引暗証番号
    --
    --
    txtSellCode = TL.pack . show $ sellOrderCode order
    --
    cannotgetSellOrderPageEx = BB.UnexpectedHTMLException
        "株式売り注文ページを受け取れていません。"
    --
    --
    donothaveStockSellEx = BB.DontHaveStocksToSellException $ TL.concat
        [ "証券コード", txtSellCode, "の株式を所有してないので売れません" ]
    --
    --
    cannotgotoSellPageEx = BB.UnexpectedHTMLException $ TL.concat
        [ "証券コード", txtSellCode, "の株式注文ページに行けません" ]
    --
    --
    cannotgetConfirmPageEx = BB.UnexpectedHTMLException
        "注文確認ページを受け取れていません。"
    --
    --
    cannotgotoConfirmPageEx = BB.UnexpectedHTMLException $ TL.concat
        [ "証券コード", txtSellCode, "の注文確認ページに行けません" ]
    --
    --
    cannotgotoAcceptedPageEx = BB.UnexpectedHTMLException $ TL.concat
        [ "証券コード", txtSellCode, "の注文終了ページに行けません" ]

