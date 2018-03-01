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
Module      :  SBIsecCoJp.Scraper
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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module SBIsecCoJp.Scraper
    ( MarketInfo(..)
    , MarketInfoPage(..)
    , TopPage(..)
    , AccMenuPage(..)
    , PurchaseMarginListPage(..)
    , PurchaseMarginDetailPage(..)
    , HoldStockDetailLink(..)
    , HoldStockListPage(..)
    , HoldStockDetailPage(..)
    , topPage
    , accMenuPage
    , purchaseMarginListPage
    , purchaseMarginDetailPage
    , formLoginPage
    , holdStockListPage
    , holdStockDetailPage
    , marketInfoPage
    ) where
import           Control.Exception.Safe
import           Control.Monad                    as M
import           Control.Monad                    ((>=>))
import qualified Data.Char                        as C
import           Data.Int                         (Int32)
import qualified Data.Maybe                       as Mb
import           Data.Monoid                      (mempty, (<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.Int       as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import           System.IO                        (Newline (..), nativeNewline)
import qualified Text.HTML.DOM                    as H
import           Text.XML.Cursor                  (($/), ($//), (&/), (&//),
                                                   (&|))
import qualified Text.XML.Cursor                  as X

import qualified GenScraper                       as GS
import           ModelDef                         (TickerSymbol (..))

-- |
-- マーケット情報
data MarketInfo = MarketInfo
    { miPrice       :: Double   -- ^ 現在値
    , miMonth       :: Int      -- ^ 月
    , miDay         :: Int      -- ^ 日
    , miHour        :: Int      -- ^ 時間
    , miMin         :: Int      -- ^ 時間
    , miDifference  :: Double   -- ^ 前日比
    , miDiffPercent :: Double   -- ^ 前日比(%)
    , miOpen        :: Double   -- ^ 始値
    , miHigh        :: Double   -- ^ 高値
    , miLow         :: Double   -- ^ 安値
    } deriving Eq

-- |
-- マーケット情報ページの内容
data MarketInfoPage
    = MInikkei225         (Maybe MarketInfo)
    | MInikkei225future   (Maybe MarketInfo)
    | MItopix             (Maybe MarketInfo)
    deriving Eq

-- |
-- MarketInfoのshow
instance Show MarketInfo where
    show MarketInfo{..} =
        TL.unpack . TLB.toLazyText $ mempty
        <> "現在値: " <> frf miPrice
        <> " (" <> md <> " " <> time <> ")" <> newline
        <> "前日比: " <> frf miDifference <> " " <> frf miDiffPercent <> "%" <> newline
        <> "open " <> frf miOpen <> newline
        <> "high " <> frf miHigh <> newline
        <> "low " <> frf miLow
        where
        frf = TLB.formatRealFloat TLB.Fixed (Just 2)
        md = TLB.decimal miMonth <> "/" <> TLB.decimal miDay
        time = TLB.decimal miHour <> ":" <> TLB.decimal miMin

-- |
-- MarketInfoPageのshow
instance Show MarketInfoPage where
    show mip =
        let (t,v) = case mip of
                    (MInikkei225 info)       -> ("日経平均", info)
                    (MInikkei225future info) -> ("日経平均先物", info)
                    (MItopix info)           -> ("TOPIX", info)
        in
        TL.unpack . TLB.toLazyText $
        TLB.fromText t <> newline <>
        maybe "マーケット情報ありません" (TLB.fromString . show) v
        <> newline

-- |
-- 改行文字
newline :: TLB.Builder
newline =
    TLB.fromString $
    case nativeNewline of
        LF   -> "\n"
        CRLF -> "\r\n"

-- |
-- トップページの内容
newtype TopPage = TopPage
    { getTopPage :: [GS.AnchorTag]
    } deriving (Eq, Show)

-- |
-- 口座管理ページの内容
newtype AccMenuPage = AccMenuPage
    { getAccMenuPage :: [GS.AnchorTag]
    } deriving (Eq, Show)

-- |
-- 買付余力ページの内容
newtype PurchaseMarginListPage = PurchaseMarginListPage
    { getPurchaseMarginListPage :: [GS.AnchorTag]
    } deriving (Eq, Show)

-- |
-- 買付余力詳細ページの内容
data PurchaseMarginDetailPage = PurchaseMarginDetailPage
    { pmdUserid       :: T.Text    -- ^ ユーザーID
    , pmdDay          :: T.Text    -- ^ 受渡日
    , pmdMoneyToSpare :: Int32     -- ^ 買付余力
    , pmdCashBalance  :: Int32     -- ^ 保証金現金
    } deriving (Eq, Show)

-- |
-- 保有証券詳細ページの内容
data HoldStockDetailPage = HoldStockDetailPage
    { hsdUserid        :: T.Text                    -- ^ ユーザーID
    , hsdMDHourMin     :: Maybe (Int,Int,Int,Int)   -- ^ 時間(取引がない場合はNothing)
    , hsdTicker        :: TickerSymbol              -- ^ ティッカーシンボル
    , hsdCaption       :: T.Text                    -- ^ 名前
    , hsdDiff          :: Maybe Double              -- ^ 前日比(取引がない場合はNothing)
    , hsdCount         :: Int32                     -- ^ 保有数
    , hsdPurchasePrice :: Double                    -- ^ 取得単価
    , hsdPrice         :: Double                    -- ^ 現在値
    } deriving (Eq, Show)

-- |
-- 保有証券詳細ページへのリンク
newtype HoldStockDetailLink = HoldStockDetailLink
    { getHoldStockDetailLink :: [GS.AnchorTag]
    } deriving (Eq, Show)

-- |
-- 保有証券一覧ページの内容
data HoldStockListPage = HoldStockListPage
    { hslUserid     :: T.Text               -- ^ ユーザーID
    , hslNthPages   :: T.Text               -- ^ 1～2件(2件中)等
    , hslEvaluation :: Double               -- ^ 評価合計
    , hslProfit     :: Double               -- ^ 損益合計
    , hslLinks      :: HoldStockDetailLink  -- ^ 保有証券詳細ページへのリンク
    } deriving (Eq, Show)

-- |
-- ログインページをスクレイピングする関数
formLoginPage :: MonadThrow m => TL.Text -> m GS.FormTag
formLoginPage html =
    let tag = X.fromDocument (H.parseLT html)
            $// X.element "form" >=> X.attributeIs "name" "form1"
            &| GS.takeFormTag
    in
    case concat tag of
    [] -> GS.throwScrapingEx "\"form1\" form or \"action\" attribute is not present."
    x:_ -> pure x

-- |
-- トップページをスクレイピングする関数
topPage :: TL.Text -> TopPage
topPage =
    TopPage . GS.takeAnchorTag . X.fromDocument . H.parseLT

-- |
-- 口座管理ページをスクレイピングする関数
accMenuPage :: TL.Text -> AccMenuPage
accMenuPage =
    AccMenuPage . GS.takeAnchorTag . X.fromDocument . H.parseLT

-- |
-- 買付余力ページをスクレイピングする関数
purchaseMarginListPage :: TL.Text -> PurchaseMarginListPage
purchaseMarginListPage html =
    PurchaseMarginListPage (concatMap GS.takeAnchorTag table)
    where
    --
    -- <div class="titletext">買付余力</div> 以下のtable
    table :: [X.Cursor]
    table =
        X.fromDocument (H.parseLT html)
        $// X.attributeIs "class" "titletext"
        >=> X.followingSibling >=> X.laxElement "div"
        &/ X.laxElement "table"

-- |
-- 買付余力詳細ページをスクレイピングする関数
purchaseMarginDetailPage :: TL.Text -> Maybe PurchaseMarginDetailPage
purchaseMarginDetailPage html =
    pack =<< Mb.listToMaybe table
    where
    --
    -- <div class="titletext">買付余力詳細</div> 以下のtable
    table :: [X.Cursor]
    table =
        X.fromDocument (H.parseLT html)
        $// X.attributeIs "class" "titletext"
        >=> X.followingSibling >=> X.element "div"
        &/ X.element "table" &/ X.element "tr" &/ X.element "td"
    --
    --
    userid :: X.Cursor -> Maybe T.Text
    userid cursor =
        Just . T.strip =<< Mb.listToMaybe . drop 1 =<< pure (cursor $/ X.content)
    --
    --
    pack :: X.Cursor -> Maybe PurchaseMarginDetailPage
    pack cursor =
        let xs = cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &// X.content
        in do
        uid <- userid cursor
        day <- Just . T.strip =<< Mb.listToMaybe . drop 1 =<< pure xs
        mts <- GS.toDecimal . T.strip =<< Mb.listToMaybe . drop 3 =<< pure xs
        cb <- GS.toDecimal . T.strip =<< Mb.listToMaybe . drop 5 =<< pure xs
        Just PurchaseMarginDetailPage
            { pmdUserid = uid
            , pmdDay = day
            , pmdMoneyToSpare = mts
            , pmdCashBalance = cb
            }

-- |
-- 保有証券一覧ページをスクレイピングする関数
holdStockListPage :: TL.Text -> Maybe HoldStockListPage
holdStockListPage html =
    pack =<< Mb.listToMaybe table
    where
    --
    -- <div class="titletext">保有証券一覧</div> 以下のtable
    table :: [X.Cursor]
    table =
        X.fromDocument (H.parseLT html)
        $// X.attributeIs "class" "titletext"
        >=> X.followingSibling >=> X.element "table" &/ X.element "tr" &/ X.element "td"
    --
    --
    userid :: X.Cursor -> Maybe T.Text
    userid cursor =
        Just . T.strip =<< Mb.listToMaybe . drop 1 =<< pure (cursor $/ X.content)
    --
    --
    nthPages :: X.Cursor -> Maybe T.Text
    nthPages cursor =
        let xs = cursor
                $/ X.element "table" &/ X.element "tr"
                &/ X.element "form" &/ X.element "td"
                &// X.content
        in
        case xs of
            [] -> Nothing
            _  -> Just . T.concat $ map T.strip xs
    --
    --
    summary :: X.Cursor -> Maybe (T.Text, T.Text)
    summary cursor =
        let xs = cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
                >=> X.descendant
            ys = concatMap X.content xs
        in do
        a <- Mb.listToMaybe $ drop 1 ys
        b <- Mb.listToMaybe $ drop 4 ys
        Just (T.strip a, T.strip b)
    --
    --
    links cursor =
        let xs = cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
            ys = concatMap GS.takeAnchorTag xs
            dropKehai = filter ((/=) "気配" . GS.aText)
        in
        HoldStockDetailLink $ dropKehai ys  -- 気配リンクは不要
    --
    --
    pack :: X.Cursor -> Maybe HoldStockListPage
    pack cursor = do
        uid <- userid cursor
        nth <- nthPages cursor
        sm <- summary cursor
        ev <- GS.toDouble $ fst sm
        pf <- GS.toDouble $ snd sm
        Just HoldStockListPage
            { hslUserid = uid
            , hslNthPages = nth
            , hslEvaluation = ev
            , hslProfit = pf
            , hslLinks = links cursor
            }

-- |
-- 保有証券詳細ページをスクレイピングする関数
holdStockDetailPage :: TL.Text -> Maybe HoldStockDetailPage
holdStockDetailPage html =
    pack =<< Mb.listToMaybe table
    where
    --
    -- <div class="titletext">保有証券詳細</div> 以下のtable
    table :: [X.Cursor]
    table =
        X.fromDocument (H.parseLT html)
        $// X.attributeIs "class" "titletext"
        >=> X.followingSibling >=> X.element "div"
        &/ X.element "table"
    --
    --
    userid :: X.Cursor -> Maybe T.Text
    userid cursor =
        let
            xs = filter (/= T.empty) . map T.strip $
                cursor
                $/ X.element "tr" &/ X.element "td"
                &// X.content
        in
        Mb.listToMaybe xs
    --
    --
    tickerCaption :: X.Cursor -> Maybe (TickerSymbol, T.Text)
    tickerCaption cursor =
        let xs = cursor
                $/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &/ X.content
        in
        case xs of
            [] -> Nothing
            _  ->
                let
                    spaceSeparated = T.concat $ map T.strip xs
                    (t,c) = T.break C.isSpace spaceSeparated
                in do
                code <- GS.toDecimal t
                Just (TSTYO code, T.strip c)
    --
    -- (現在値, 時間)
    priceMDHourMin :: X.Cursor -> Maybe (Double, Maybe (Int,Int,Int,Int))
    priceMDHourMin cursor =
        let
            xs = filter (/= T.empty) . map T.strip $
                cursor
                $/ X.element "tr" &/ X.element "td"
                &// X.content
        in do
        -- 現在値
        pr <- GS.toDouble =<< Mb.listToMaybe . drop 4 =<< pure xs
        -- 時間
        chunk <- T.init . T.tail . T.dropWhile (/= '(')
                <$> (Mb.listToMaybe . drop 5 $ xs)
        case M.mapM GS.toDecimal . T.split (`elem` ['/',' ',':']) . T.strip $ chunk of
            Just [month,day,hour,minute] -> Just (pr, Just (month,day,hour,minute))
            _ -> Just (pr, Nothing)
    --
    --
    diffCountGain :: X.Cursor -> Maybe (Maybe Double, Int32, Double)
    diffCountGain cursor =
        let
            xs = cursor
                $/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
            ys = filter (/= T.empty)
                . map T.strip
                . concatMap ($// X.content)
                . take 1 $ drop 2 xs
        in do
        -- "-" は現在取引無し
        d <- \case{"-"->Nothing; x->GS.toDouble x} <$> (Mb.listToMaybe . drop 1 $ ys)
        c <- GS.toDecimal =<< Mb.listToMaybe . drop 3 =<< pure ys
        g <- GS.toDouble =<< Mb.listToMaybe . drop 5 =<< pure ys
        Just (d, c, g)
    --
    --
    pack :: X.Cursor -> Maybe HoldStockDetailPage
    pack cursor = do
        uid <- userid cursor
        (ts,ca) <- tickerCaption cursor
        (pr,at) <- priceMDHourMin cursor
        (d,c,g) <- diffCountGain cursor
        Just HoldStockDetailPage
            { hsdUserid = uid
            , hsdMDHourMin = at
            , hsdTicker = ts
            , hsdCaption = ca
            , hsdDiff = d
            , hsdCount = c
            , hsdPurchasePrice = pr - g / realToFrac c
            , hsdPrice = pr
            }

-- |
-- マーケット情報ページをスクレイピングする関数
marketInfoPage :: TL.Text -> Maybe MarketInfoPage
marketInfoPage html =
    let table = [("国内指標", MInikkei225)
                ,("日経平均", MInikkei225)
                ,("日経平均先物", MInikkei225future)
                ,("TOPIX", MItopix)
                ]
    in do
    cap <- caption
    fn <- lookup cap table
    Just $ fn marketInfo
    where
    --
    --
    marketInfo :: Maybe MarketInfo
    marketInfo = do
        (pr,month,day,hour,minute,df,dp) <- priceMonthDayHourMinDiffPercent =<< Mb.listToMaybe . drop 1 =<< pure tables
        (o,h,l) <- openHighLow =<< Mb.listToMaybe . drop 2 =<< pure tables
        Just MarketInfo
            { miPrice = pr
            , miMonth = month
            , miDay = day
            , miHour = hour
            , miMin = minute
            , miDifference = df
            , miDiffPercent = dp
            , miOpen = o
            , miHigh = h
            , miLow = l
            }
    --
    --
    root = X.fromDocument (H.parseLT html)
    --
    -- <div class="titletext"></div> 以下の
    -- <table><tr><td><form><table>
    tables :: [X.Cursor]
    tables =
        X.fromDocument (H.parseLT html)
        $// X.attributeIs "class" "titletext"
        >=> X.followingSibling >=> X.element "table" &/ X.element "tr" &/ X.element "td"
        &/ X.element "form" &/ X.element "table"
    --
    --
    caption :: Maybe T.Text
    caption =
        Mb.listToMaybe $
        root $// X.attributeIs "class" "titletext" &// X.content
    --
    --
    priceMonthDayHourMinDiffPercent :: X.Cursor -> Maybe (Double, Int, Int, Int, Int, Double, Double)
    priceMonthDayHourMinDiffPercent cursor = do
        let tds = map T.strip $
                    cursor
                    $/ X.element "tr" &/ X.element "td" &// X.content
        -- 現在値
        pr <- GS.toDouble =<< Mb.listToMaybe . drop 1 =<< pure tds
        -- 時間
        chunk <- T.init . T.tail . T.strip <$> (Mb.listToMaybe . drop 2 $ tds)
        parts <- M.mapM GS.toDecimal . T.split (`elem` ['/',' ',':']) . T.strip $ chunk
        M.guard (length parts == 4)
        let [month, day, hour, minute] = parts
        -- 前日比
        df <- GS.toDouble =<< Mb.listToMaybe . drop 4 =<< pure tds
        -- 前日比(%)
        dp <- GS.toDouble =<< Mb.listToMaybe . drop 6 =<< pure tds
        Just (pr, month, day, hour, minute, df, dp)
    --
    --
    openHighLow :: X.Cursor -> Maybe (Double, Double, Double)
    openHighLow cursor = do
        let tds = map T.strip $
                    cursor
                    $/ X.element "tr" &/ X.element "td" &// X.content
        -- 始値
        open <- GS.toDouble =<< Mb.listToMaybe . drop 1 =<< pure tds
        -- 高値
        high <- GS.toDouble =<< Mb.listToMaybe . drop 3 =<< pure tds
        -- 安値
        low <- GS.toDouble =<< Mb.listToMaybe . drop 5 =<< pure tds
        Just (open, high, low)

