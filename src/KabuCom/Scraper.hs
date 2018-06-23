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
Module      :  KabuCom.Scraper
Description :  Scraping a web page
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

スクレイピングするモジュールです。

>>> :set -XOverloadedStrings
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module KabuCom.Scraper
    ( TopPage(..)
    , DayHourMinute(..)
    , HourMinute(..)
    , StockClassification(..)
    , toStockClassification
    , DailyStockPrice(..)
    , StockDetailPage(..)
    , StockPositionItem(..)
    , StockPositionListPage(..)
    , PurchaseMarginPage(..)
    , formLoginPage
    , topPage
    , purchaseMarginPage
    , stockPositionListPage
    , stockDetailPage
    ) where
import qualified Control.Arrow          as A
import           Control.Exception.Safe
import           Control.Monad          ((>=>))
import qualified Control.Monad          as M
import           Data.Int               (Int32, Int64)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Time              as Time
import qualified Text.HTML.DOM          as DOM
import           Text.XML.Cursor        (($/), ($//), (&/), (&|))
import qualified Text.XML.Cursor        as X

import qualified GenScraper             as GS
import           ModelDef               (TickerSymbol (..))
import           Scheduling             (AsiaTokyoDay (..))

-- |
-- トップページの内容
newtype TopPage = TopPage
    { getTopPage :: [GS.AnchorTag]
    } deriving (Eq, Show)

-- |
-- トップページをスクレイピングする関数
topPage :: TL.Text -> TopPage
topPage html =
    TopPage (GS.takeAnchorTag root)
    where
    root = X.fromDocument (DOM.parseLT html)

-- |
-- 日時分
newtype DayHourMinute = DayHourMinute
    { getDayHourMinute :: (AsiaTokyoDay, HourMinute)
    } deriving (Eq, Show)

-- |
-- 時分
newtype HourMinute = HourMinute
    { getHourMinute :: (Int, Int)
    } deriving (Eq, Show)

-- |
-- 貸借信用区分
data StockClassification = SC貸借銘柄 | SC融資銘柄 | SC一般信用銘柄 deriving Eq
instance Show StockClassification where
    show SC貸借銘柄 = "貸借銘柄"
    show SC融資銘柄 = "融資銘柄"
    show SC一般信用銘柄 = "一般信用銘柄"

toStockClassification :: MonadThrow m => T.Text -> m StockClassification
toStockClassification = \case
    "[貸]" -> pure SC貸借銘柄
    "[融]" -> pure SC融資銘柄
    "[一]" -> pure SC一般信用銘柄
    t ->
        let t' = T.unpack t
        in
        GS.throwScrapingEx $ "toStockClassification: unknown of \"" ++ t' ++ "\""

-- |
-- 株価(日足)
data DailyStockPrice = DailyStockPrice
    { dspDay    :: AsiaTokyoDay
    , dspOpen   :: Maybe Double
    , dspHigh   :: Maybe Double
    , dspLow    :: Maybe Double
    , dspClose  :: Maybe Double
    , dspDiff   :: Maybe Double
    , dspVolume :: Int64
    } deriving (Eq, Show)

type PriceAt t = (Double, t)
type PriceAtHM = PriceAt HourMinute
type PriceAtDHM = PriceAt DayHourMinute

-- |
-- 個別銘柄詳細ページの内容
data StockDetailPage = StockDetailPage
    { sdpChartLink      :: GS.AnchorTag         -- ^ チャートリンク
    , sdpBoardLink      :: GS.AnchorTag         -- ^ 板リンク
    , sdpBuyLink        :: GS.AnchorTag         -- ^ 買いリンク
    , sdpSellLink       :: GS.AnchorTag         -- ^ 売りリンク
    , sdpPetitBuyLink   :: Maybe GS.AnchorTag   -- ^ プチ株買いリンク
    , sdpPetitSellLink  :: Maybe GS.AnchorTag   -- ^ プチ株売りリンク
    , sdpAskPrice       :: Int32                -- ^ 約定金額
    , sdpTicker         :: TickerSymbol         -- ^ ティッカーシンボル
    , sdpClass          :: StockClassification  -- ^ 貸借銘柄／信用銘柄
    , sdpCaption        :: T.Text               -- ^ 銘柄名
    , sdpDay            :: AsiaTokyoDay         -- ^ 日付
    , sdpPrice          :: Maybe PriceAtHM      -- ^ 現在値
    , sdpDiff           :: Maybe Double         -- ^ 前日比
    , sdpDiffPc         :: Maybe Double         -- ^ 前日比(%)
    , sdpCloseYesterday :: Maybe Double         -- ^ 前日終値
    , sdpOpen           :: Maybe PriceAtHM      -- ^ 始値
    , sdpHigh           :: Maybe PriceAtHM      -- ^ 高値
    , sdpLow            :: Maybe PriceAtHM      -- ^ 安値
    , sdpOpenAfternoon  :: Maybe PriceAtHM      -- ^ 後場始値
    , sdpStdPrice       :: Maybe PriceAtDHM     -- ^ 計算基準値
    , sdpDailyHistories :: [DailyStockPrice]    -- ^ 時系列
    } deriving (Eq, Show)

-- |
-- 保有証券の内容
data StockPositionItem = StockPositionItem
    { spDetailAnchor        :: GS.AnchorTag -- ^ 明細
    , spCaptionAnchor       :: GS.AnchorTag -- ^ 名前
    , spTicker              :: TickerSymbol -- ^ ティッカーシンボル
    , spCount               :: Int32        -- ^ 保有数
    , spPrice               :: Double       -- ^ 現在値
    , spPurchasePrice       :: Double       -- ^ 取得単価
    , spAmountPrice         :: Double       -- ^ 評価金額
    , spAmountPurchasePrice :: Double       -- ^ 取得金額
    , spProfit              :: Double       -- ^ 損益
    , spProfitPc            :: Double       -- ^ 損益(%)
    , spSellOrderAnchor     :: GS.AnchorTag -- ^ 売り
    , spBuyOrderAnchor      :: GS.AnchorTag -- ^ 買い
    } deriving (Eq, Show)

-- |
-- 残高照会ページの内容
data StockPositionListPage = StockPositionListPage
    { splCaption    :: T.Text               -- ^ タイトル
    , splMDHourMin  :: (Int,Int,Int,Int)    -- ^ 時間
    , splEvaluation :: Double               -- ^ 評価合計
    , splProfit     :: Double               -- ^ 損益合計
    , splProfitPc   :: Double               -- ^ 損益合計(%)
    , splPositions  :: [StockPositionItem]  -- ^ 保有証券
    } deriving (Eq, Show)

-- |
-- 買付余力ページの内容
data PurchaseMarginPage = PurchaseMarginPage
    { pmMoneyToSpare :: Int32     -- ^ 買付余力
    , pmCashBalance  :: Int32     -- ^ 保証金現金
    } deriving (Eq, Show)

-- |
-- ログインページをスクレイピングする関数
formLoginPage :: MonadThrow m => TL.Text -> m GS.FormTag
formLoginPage html =
    let tags = X.fromDocument (DOM.parseLT html)
                $// X.laxElement "form" >=> X.attributeIs "NAME" "xyz"
                &| GS.takeFormTag
    in
    case concat tags of
    []  -> GS.throwScrapingEx "\"xyz\" form or \"action\" attribute is not present."
    x:_ -> pure x

-- |
-- 買付余力ページをスクレイピングする関数
purchaseMarginPage :: MonadThrow m => TL.Text -> m PurchaseMarginPage
purchaseMarginPage html =
    case tables of
    x:_ -> pack x
    []  -> GS.throwScrapingEx "purchaseMarginPage: list is empty"
    where
    --
    --
    root = X.fromDocument $ DOM.parseLT html
    --
    --
    tables :: [X.Cursor]
    tables =
        let xs r = r
                    $/ X.laxElement "body"
                    &/ X.laxElement "table"
            ys x = x
                    $/ X.laxElement "tr" &/ X.laxElement "td"
                    &/ X.laxElement "table"
            zs y = y
                    $/ X.laxElement "tr" &/ X.laxElement "td"
        in
        zs =<< drop 2 . ys =<< take 1 . drop 1 . xs =<< pure root
    --
    --
    pack :: MonadThrow m => X.Cursor -> m PurchaseMarginPage
    pack cursor =
        case scraping cursor of
        [_, _, mts, _, cb] -> do
            mts' <- GS.toDecimal mts
            cb' <- GS.toDecimal cb
            pure PurchaseMarginPage
                { pmMoneyToSpare = mts'
                , pmCashBalance = cb'
                }
        t -> GS.throwScrapingEx $ "purchaseMarginPage: length mismatch (" ++ show (length t) ++ ")"

-- |
-- 保有証券一覧ページをスクレイピングする関数
stockPositionListPage :: MonadThrow m => TL.Text -> m StockPositionListPage
stockPositionListPage html =
    case take 7 tables of
    [headline, _, summary, _, _, _, ps] -> do
        (cap, tim) <- captionMonthDayHourMin headline
        (ev, pro, ppc) <- evaluationProfitProfitPc summary
        ps' <- positions ps
        pure StockPositionListPage
            { splCaption = cap
            , splMDHourMin = tim
            , splEvaluation = ev
            , splProfit = pro
            , splProfitPc = ppc
            , splPositions = ps'
            }
    t -> GS.throwScrapingEx $ "stockPositionListPage: length mismatch (" ++ show (length t) ++ ")"
    where
    --
    --
    root = X.fromDocument $ DOM.parseLT html
    --
    --
    tables :: [X.Cursor]
    tables =
        let xs r = r
                    $/ X.laxElement "body"
                    &/ X.laxElement "table"
            ys x = x
                    $/ X.laxElement "tr" &/ X.laxElement "td"
                    &/ X.laxElement "table" &/ X.laxElement "tr" &/ X.laxElement "td"
            zs y = y
                    $/ X.laxElement "table"
        in
        zs =<< take 1 . ys =<< take 1 . drop 1 . xs =<< pure root
    --
    --
    captionMonthDayHourMin :: MonadThrow m => X.Cursor -> m (T.Text, (Int,Int,Int,Int))
    captionMonthDayHourMin cursor =
        case scraping cursor of
        [cap, txt] -> (,) <$> pure cap <*> mdhm txt
        t -> GS.throwScrapingEx $ "captionMonthDayHourMin: length mismatch ("++ show (length t) ++ ")"
        where
        --
        --
        mdhm x =
            case T.split (`elem` ['/',' ',':']) x of
            [mo, da, ho, mi] ->
                (,,,)   <$> GS.toDecimal mo <*> GS.toDecimal da
                        <*> GS.toDecimal ho <*> GS.toDecimal mi
            t -> GS.throwScrapingEx $ "captionMonthDayHourMin: length mismatch ("++ show (length t) ++ ")"
    --
    --
    evaluationProfitProfitPc :: MonadThrow m => X.Cursor -> m (Double, Double, Double)
    evaluationProfitProfitPc cursor =
        case scraping cursor of
        [_, ev, _, pro, _, ppc, _] ->
            (,,) <$> GS.toDouble ev <*> GS.toDouble pro <*> GS.toDouble ppc
        t ->
            GS.throwScrapingEx $ "evaluationProfitProfitPc: length mismatch (" ++ show (length t) ++ ")"
    --
    --
    positions :: MonadThrow m => X.Cursor -> m [StockPositionItem]
    positions cursor =
        M.mapM stockPositionItem $ drop 1 (cursor $/ X.laxElement "tr")
    --
    --
    stockPositionItem :: MonadThrow m => X.Cursor -> m StockPositionItem
    stockPositionItem cursor = do
        (det, cap, sell, buy) <- anchor4
        case scraping cursor of
            [_, cod, _, cnt, pr, ppr, ampr, amppr, pro, _, ppc, _] -> do
                cod' <- GS.toDecimal . T.init . T.tail $ cod
                cnt' <- GS.toDecimal cnt
                pr' <- GS.toDouble pr
                ppr' <- GS.toDouble ppr
                ampr' <- GS.toDouble ampr
                amppr' <- GS.toDouble amppr
                pro' <- GS.toDouble pro
                ppc' <- GS.toDouble ppc
                pure StockPositionItem
                    { spDetailAnchor = det
                    , spCaptionAnchor = cap
                    , spTicker = TSTYO cod'
                    , spCount = cnt'
                    , spPrice = pr'
                    , spPurchasePrice = ppr'
                    , spAmountPrice = ampr'
                    , spAmountPurchasePrice = amppr'
                    , spProfit = pro'
                    , spProfitPc = ppc'
                    , spSellOrderAnchor = sell
                    , spBuyOrderAnchor = buy
                    }
            t -> GS.throwScrapingEx $ "stockPositionItem: length mismatch ("++ show (length t) ++ ")"
        where
        --
        --
        anchor4 = case GS.takeAnchorTag cursor of
            [a1, a2, a3, a4] -> pure (a1, a2, a3, a4)
            t -> GS.throwScrapingEx $ "anchor4: length mismatch ("++ show (length t) ++ ")"

-- |
-- 個別銘柄詳細ページをスクレイピングする関数
stockDetailPage :: MonadThrow m => TL.Text -> m StockDetailPage
stockDetailPage html =
    case go html of
    Right a -> pure a
    Left _ ->
        --
        -- "※この銘柄は取引制限銘柄です。"
        -- "※この銘柄は取引注意銘柄です。"
        -- が注入されたページのHTMLは壊れているので修復する。
        --
        -- 不必要な情報なのでこの行ごと消去する。
        let text = "<tr><td valign=\"middle\" align=\"left\" colspan=\"2\"><br><b><FONT SIZE=2>※この銘柄は取引"
            patcher = TL.unlines . filter (not . TL.isPrefixOf text) . TL.lines
        in
        go $ patcher html
    where
    --
    --
    go html' =
        let tables = X.fromDocument (DOM.parseLT html')
                        $/ X.laxElement "body"
                        &/ X.laxElement "table"
        in
        case (tables :: [X.Cursor]) of
        [_, headline, subhead, instruments, article, _, history, _, _] -> do
            (chl, bol, buyl, sell, pbl, psl, ask) <- chartBoardBuySellPebuyPesellAsk headline
            --
            (ts, cls, cap) <- tickerClassCaption subhead
            --
            day <- finantialInstruments instruments
            --
            (pri, dif, dpc, cly, opn, hig, low, oaf, std) <- leftTable article
            --
            hist <- dailyStockPrices history
            --
            pure StockDetailPage
                { sdpChartLink = chl
                , sdpBoardLink = bol
                , sdpBuyLink = buyl
                , sdpSellLink = sell
                , sdpPetitBuyLink = pbl
                , sdpPetitSellLink = psl
                , sdpAskPrice = ask
                , sdpTicker = ts
                , sdpClass = cls
                , sdpCaption = cap
                , sdpDay = day
                , sdpPrice = pri
                , sdpDiff = dif
                , sdpDiffPc = dpc
                , sdpCloseYesterday = cly
                , sdpOpen = opn
                , sdpHigh = hig
                , sdpLow = low
                , sdpOpenAfternoon = oaf
                , sdpStdPrice = std
                , sdpDailyHistories = hist
                }
        t -> GS.throwScrapingEx $ "stockDetailPage: length mismatch ("++ show (length t) ++ ")"
    --
    --
    leftTableRows :: X.Cursor -> [X.Cursor]
    leftTableRows cursor =
        cursor
        $/ X.laxElement "tr"
        &/ X.laxElement "td"
        &/ X.laxElement "table"
        &/ X.laxElement "tr"
    --
    --
    chartBoardBuySellPebuyPesellAsk :: MonadThrow m
        => X.Cursor
        -> m( GS.AnchorTag, GS.AnchorTag, GS.AnchorTag, GS.AnchorTag
            , Maybe GS.AnchorTag, Maybe GS.AnchorTag, Int32)
    chartBoardBuySellPebuyPesellAsk cursor =
        case concatMap GS.takeAnchorTag $ take 1 tds of
            -- 通常の銘柄
            [a1, a2, a3, a4, a5, a6] ->
                (,,,,,,) a1 a2 a3 a4 (Just a5) (Just a6) <$> askPrice
            -- プチ株がない銘柄の場合
            [a1, a2, a3, a4] ->
                (,,,,,,) a1 a2 a3 a4 Nothing Nothing <$> askPrice
            --
            t -> GS.throwScrapingEx $ "chartBoardBuySellPebuyPesellAsk: length mismatch ("++ show (length t) ++ ")"
        where
        --
        --
        tds = cursor $/ X.laxElement "tr" &/ X.laxElement "td"
        --
        --
        askPrice =
            case concatMap scraping $ take 1 tds of
            -- このリストにはこんな情報が入っている
            -- ["約定金額", 値, "円"]
            [_, v, _] -> GS.toDecimal v
            t -> GS.throwScrapingEx $ "askPrice: length mismatch ("++ show (length t) ++ ")"
    --
    --
    leftTable :: MonadThrow m
        => X.Cursor
        -> m( Maybe PriceAtHM, Maybe Double, Maybe Double, Maybe Double
            , Maybe PriceAtHM, Maybe PriceAtHM, Maybe PriceAtHM, Maybe PriceAtHM, Maybe PriceAtDHM)
    leftTable cursor =
        case leftTableRows cursor of
        --
        -- 前場
        [a1, a2, a3, a4, a5, a6, a7, a9] ->
            (,,,,,,,,)  <$> valueHourMin a1    -- 現在値
                        <*> value a2           -- 前日比
                        <*> value a3           -- 前日比(%)
                        <*> value a4           -- 前日終値
                        <*> valueHourMin a5    -- 始値
                        <*> valueHourMin a6    -- 高値
                        <*> valueHourMin a7    -- 安値
                        <*> pure Nothing       -- 後場始値は後場になるまでない情報
                        <*> valueDayHourMin a9 -- 計算基準値
        --
        -- 後場
        [a1, a2, a3, a4, a5, a6, a7, a8, a9] ->
            (,,,,,,,,)  <$> valueHourMin a1     -- 現在値
                        <*> value a2            -- 前日比
                        <*> value a3            -- 前日比(%)
                        <*> value a4            -- 前日終値
                        <*> valueHourMin a5     -- 始値
                        <*> valueHourMin a6     -- 高値
                        <*> valueHourMin a7     -- 安値
                        <*> valueHourMin a8     -- 後場始値
                        <*> valueDayHourMin a9  -- 計算基準値
        --
        -- 失敗
        t -> GS.throwScrapingEx $ "leftTable: length mismatch ("++ show (length t) ++ ")"
    --
    --
    value :: MonadThrow m => X.Cursor -> m (Maybe Double)
    value cursor =
        case scraping cursor of
        -- このリストにはこんな情報が入っている
        --  [説明, 値]
        [_, v]  | T.isPrefixOf "--" v -> pure Nothing           -- 寄り付いていない
                | otherwise -> Just <$> GS.toDouble v
        t      -> GS.throwScrapingEx $ "value: length mismatch ("++ show (length t) ++ ")"
    --
    --
    valueHourMin :: MonadThrow m => X.Cursor -> m (Maybe (PriceAt HourMinute))
    valueHourMin cursor =
        case scraping cursor of
        -- このリストにはこんな情報が入っている
        --  [説明, 値, 時分]
        [_, v, hm]  | T.isPrefixOf "(--)" hm -> pure Nothing    -- 寄り付いていない
                    | otherwise ->
            let hm' = T.init $ T.tail hm
            in
            pure $ (,) <$> GS.toDouble v <*> parseHM hm'
        t -> GS.throwScrapingEx $ "valueHourMin: length mismatch ("++ show (length t) ++ ")"
    --
    --
    valueDayHourMin :: MonadThrow m => X.Cursor -> m (Maybe (PriceAt DayHourMinute))
    valueDayHourMin cursor =
        case scraping cursor of
        [_, v, tim] | T.isPrefixOf "(--)" v -> pure Nothing     -- 寄りついていない
                    | otherwise ->
            case T.words . T.init . T.tail $ tim of
                [d, t] ->
                    let dhm = (,) <$> parseDay d <*> parseHM t
                    in
                    pure $ (,) <$> GS.toDouble v <*> fmap DayHourMinute dhm
                t -> GS.throwScrapingEx $ "valueDayHourMin: length mismatch ("++ show (length t) ++ ")"
        t -> GS.throwScrapingEx $ "valueDayHourMin: length mismatch ("++ show (length t) ++ ")"
    --
    --
    tickerClassCaption :: MonadThrow m => X.Cursor -> m (TickerSymbol, StockClassification, T.Text)
    tickerClassCaption cursor =
        case concatMap scraping (take 1 tds) of
        -- このリストにはこんな情報が入っている
        --  証券コード "スペース" [貸]
        --  ”銘柄名”
        -- "(市場／*東京 ETF/ETN)"
        codeAndClass : caption : _ ->
            case T.words codeAndClass of
                [code, klass] ->
                    (,,) <$> fmap TSTYO (GS.toDecimal code)
                         <*> toStockClassification klass
                         <*> pure caption
                t -> GS.throwScrapingEx $ "tickerClassCaption: length mismatch ("++ show (length t) ++ ")"
        t -> GS.throwScrapingEx $ "tickerClassCaption: length mismatch ("++ show (length t) ++ ")"
        where
        --
        --
        tds = cursor $/ X.laxElement "tr" &/ X.laxElement "td"
    --
    -- 取扱商品テーブルから日付を取り出す
    finantialInstruments :: MonadThrow m => X.Cursor -> m AsiaTokyoDay
    finantialInstruments cursor =
        case cursor $/ X.laxElement "tr" of
            -- [取扱商品, 時価情報]
            [_, jika] ->
                case scraping jika of
                    -- ["時価情報", 日付]
                    [_, day] -> parseDay day
                    t -> GS.throwScrapingEx $ "finantialInstruments: length mismatch ("++ show (length t) ++ ")"
            t -> GS.throwScrapingEx $ "finantialInstruments: length mismatch ("++ show (length t) ++ ")"
    -- |
    -- 日足を取り出す
    dailyStockPrices :: MonadThrow m => X.Cursor -> m [DailyStockPrice]
    dailyStockPrices cursor =
        -- 先頭はテーブルヘッダー
        M.mapM pack $ drop 1 trs
        where
        --
        --
        trs = cursor $// X.laxElement "tr"
        --
        --
        pack :: MonadThrow m => X.Cursor -> m DailyStockPrice
        pack tr =
            case concatMap scraping (tr $/ X.laxElement "td") of
            [day, o, h, l, c, d, v] -> do
                day' <- parseDay day
                v' <- GS.toDecimal v
                pure DailyStockPrice
                    { dspDay = day'
                    , dspOpen = GS.toDouble o
                    , dspHigh = GS.toDouble h
                    , dspLow = GS.toDouble l
                    , dspClose = GS.toDouble c
                    , dspDiff = GS.toDouble d
                    , dspVolume = v'
                    }
            t -> GS.throwScrapingEx $ "dailyStockPrices: length mismatch ("++ show (length t) ++ ")"

-- |
-- 時分("9:00"など)を分解する
--
-- >>> parseHM "9:00"
-- HourMinute {getHourMinute = (9,0)}
-- >>> parseHM "13:31"
-- HourMinute {getHourMinute = (13,31)}
parseHM :: MonadThrow m => T.Text -> m HourMinute
parseHM =
    fmap HourMinute
    . (\(h,m) -> (,) <$> h <*> m)
    . (GS.toDecimal A.*** GS.toDecimal . T.tail)
    . T.breakOn ":"

-- |
-- 日付を返す
--
-- >>> parseDay "2018/02/23(金)"
-- AsiaTokyoDay {getAsiaTokyoDay = 2018-02-23}
-- >>> parseDay "2018/02/23"
-- AsiaTokyoDay {getAsiaTokyoDay = 2018-02-23}
parseDay :: MonadThrow m => T.Text -> m AsiaTokyoDay
parseDay =
    fmap AsiaTokyoDay
    . Time.parseTimeM True Time.defaultTimeLocale "%Y/%m/%d"
    -- 日付のみ取り出す
    . T.unpack . fst . T.breakOn "("

-- |
-- cursor以下を再帰的に, それぞれ前後の空白を削った文字列リストのうち, 空文字列を削ったリストを返す
scraping :: X.Cursor -> [T.Text]
scraping cursor =
    filter (not . T.null) [T.strip t | t<-cursor $// X.content]

