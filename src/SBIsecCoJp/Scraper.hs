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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
module SBIsecCoJp.Scraper
    ( MarketInfoPage(..)
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
import           Control.Monad                    ((>=>))
import qualified Control.Monad                    as M
import qualified Data.Char                        as C
import           Data.Int                         (Int32)
import qualified Data.Maybe                       as Maybe
import           Data.Monoid                      (mempty, (<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.Int       as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import qualified Data.Text.Read                   as Read
import           System.IO                        (Newline (..), nativeNewline)
import qualified Text.HTML.DOM                    as H
import           Text.XML.Cursor                  (($/), ($//), (&/), (&//),
                                                   (&|))
import qualified Text.XML.Cursor                  as X

import qualified GenScraper                       as GS
import           ModelDef                         (TickerSymbol (..))

-- |
-- マーケット情報ページの内容
data MarketInfoPage = MarketInfoPage
    { miCaption     :: T.Text       -- ^ マーケット情報名
    , miPrice       :: Maybe Double -- ^ 現在値
    , miMonth       :: Maybe Int    -- ^ 時間(月)
    , miDay         :: Maybe Int    -- ^ 時間(日)
    , miHour        :: Maybe Int    -- ^ 時間(時間)
    , miMinute      :: Maybe Int    -- ^ 時間(分)
    , miDifference  :: Maybe Double -- ^ 前日比
    , miDiffPercent :: Maybe Double -- ^ 前日比(%)
    , miOpen        :: Maybe Double -- ^ 始値
    , miHigh        :: Maybe Double -- ^ 高値
    , miLow         :: Maybe Double -- ^ 安値
    } deriving Eq

-- |
-- MarketInfoPageのshow
instance Show MarketInfoPage where
    show MarketInfoPage{..} =
        let
            frf = TLB.formatRealFloat TLB.Fixed (Just 2)
        in
        TL.unpack . TLB.toLazyText $ mempty
        <> TLB.fromText miCaption <> newline
        <> "現在値: " <> maybe "-" TLB.realFloat miPrice
            <> " (" <> maybe "--" TLB.decimal miMonth
            <> "/" <> maybe "--" TLB.decimal miDay
            <> " " <> maybe "--" TLB.decimal miHour
            <> ":" <> maybe "--" TLB.decimal miMinute
            <> ")"
            <> "\n"
        <> "前日比: " <> maybe "-" frf miDifference
            <> " " <> maybe "-" frf miDiffPercent
            <> "%"
            <> newline
        <> "open " <> maybe "-" frf miOpen <> newline
        <> "high " <> maybe "-" frf miHigh <> newline
        <> "low " <> maybe "-" frf miLow <> newline

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
    { getTopPage :: [GS.TextAndHref] }
    deriving (Eq, Show)

-- |
-- 口座管理ページの内容
newtype AccMenuPage = AccMenuPage
    { getAccMenuPage :: [GS.TextAndHref] }
    deriving (Eq, Show)

-- |
-- 買付余力ページの内容
newtype PurchaseMarginListPage = PurchaseMarginListPage
    { getPurchaseMarginListPage :: [GS.TextAndHref] }
    deriving (Eq, Show)

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
    { hsdUserid        :: T.Text       -- ^ ユーザーID
    , hsdTicker        :: TickerSymbol -- ^ ティッカーシンボル
    , hsdCaption       :: T.Text       -- ^ 名前
    , hsdDiff          :: Maybe Double -- ^ 前日比(取引がない場合はNothing)
    , hsdCount         :: Int32        -- ^ 保有数
    , hsdPurchasePrice :: Double       -- ^ 取得単価
    , hsdPrice         :: Double       -- ^ 現在値
    } deriving (Eq, Show)

-- |
-- 保有証券詳細ページへのリンク
newtype HoldStockDetailLink = HoldStockDetailLink
    { getHoldStockDetailLink :: [GS.TextAndHref] }
    deriving (Eq, Show)

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
--
-- >>> toDecimal "1,000"
-- Just 1000
-- >>> toDecimal "10,000円"
-- Just 10000
-- >>> toDecimal "+1,000円"
-- Just 1000
-- >>> toDecimal "-5,000円"
-- Just (-5000)
-- >>> toDecimal "-5,a00"
-- Just (-5)
-- >>> toDecimal "-a00"
-- Nothing
toDecimal :: Integral a => T.Text -> Maybe a
toDecimal x =
    either (const Nothing) (Just . fst)
    $ Read.signed Read.decimal $ T.filter (/= ',') x

-- |
--
-- >>> toDouble "10,000.34"
-- Just 10000.34
-- >>> toDouble "+1,000.45"
-- Just 1000.45
-- >>> toDouble "-5,000"
-- Just (-5000.0)
-- >>> toDouble "-5,a00"
-- Just (-5.0)
-- >>> toDouble "-a00"
-- Nothing
toDouble :: T.Text -> Maybe Double
toDouble x =
    either (const Nothing) (Just . fst)
    $ Read.signed Read.double $ T.filter (/= ',') x

-- |
-- ログインページをスクレイピングする関数
formLoginPage :: TL.Text -> Either TL.Text GS.FormTag
formLoginPage html =
    maybe
        (Left "\"form1\" form or \"action\" attribute is not present.")
        Right
        . Maybe.listToMaybe
        . concat
        $ X.fromDocument (H.parseLT html)
        $// X.element "form" >=> X.attributeIs "name" "form1"
        &| GS.takeFormTag

-- |
-- リンクを取り出す関数
takeAnchorTag :: X.Cursor -> [GS.TextAndHref]
takeAnchorTag cursor =
    concat (cursor $// X.element "a" &| pair)
    where
    --
    --
    pair c =
        zip (txt c) (href c)
    --
    -- href属性を取り出す
    href = X.attribute "href"
    --
    -- リンクテキストを取り出す
    txt c = c $// X.content

-- |
-- トップページをスクレイピングする関数
topPage :: TL.Text -> TopPage
topPage html =
    TopPage (takeAnchorTag root)
    where
    root = X.fromDocument (H.parseLT html)

-- |
-- 口座管理ページをスクレイピングする関数
accMenuPage :: TL.Text -> AccMenuPage
accMenuPage html =
    AccMenuPage (takeAnchorTag root)
    where
    root = X.fromDocument (H.parseLT html)

-- |
-- 買付余力ページをスクレイピングする関数
purchaseMarginListPage :: TL.Text -> PurchaseMarginListPage
purchaseMarginListPage html =
    PurchaseMarginListPage (concatMap takeAnchorTag table)
    where
    --
    -- <div class="titletext">買付余力</div> 以下のtable
    table :: [X.Cursor]
    table =
        X.fromDocument (H.parseLT html)
        $// X.attributeIs "class" "titletext"
        >=> X.followingSibling >=> X.element "div"
        &/ X.element "table"

-- |
-- 買付余力詳細ページをスクレイピングする関数
purchaseMarginDetailPage :: TL.Text -> Maybe PurchaseMarginDetailPage
purchaseMarginDetailPage html =
    pack =<< Maybe.listToMaybe table
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
        Maybe.listToMaybe . drop 1 $ map T.strip (cursor $/ X.content)
    --
    --
--    pack :: X.Cursor -> Maybe T.Text
    pack cursor =
        let xs = cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &// X.content
        in do
        uid <- userid cursor
        day <- Maybe.listToMaybe . drop 1 $ map T.strip xs
        mts <- toDecimal =<<  (Maybe.listToMaybe . drop 3 $ map T.strip xs)
        cb <- toDecimal =<< (Maybe.listToMaybe . drop 5 $ map T.strip xs)
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
    pack =<< Maybe.listToMaybe table
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
        Maybe.listToMaybe . drop 1 $ map T.strip (cursor $/ X.content)
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
            _  -> Just $ T.concat $ map T.strip xs
    --
    --
    summary :: X.Cursor -> Maybe (T.Text, T.Text)
    summary cursor =
        let
            xs = cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
                >=> X.descendant
            ys = concatMap X.content xs
        in do
        a <- Maybe.listToMaybe $ drop 1 ys
        b <- Maybe.listToMaybe $ drop 4 ys
        Just (T.strip a, T.strip b)
    --
    --
    links cursor =
        HoldStockDetailLink
        ( filter (\(a,_) -> a /= "気配")    -- 気配リンクはいらない
          . concatMap takeAnchorTag
            $ cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
        )
    --
    --
    pack :: X.Cursor -> Maybe HoldStockListPage
    pack cursor = do
        uid <- userid cursor
        nth <- nthPages cursor
        sm <- summary cursor
        ev <- toDouble $ fst sm
        pf <- toDouble $ snd sm
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
    pack =<< Maybe.listToMaybe table
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
        Maybe.listToMaybe . drop 1 $ map T.strip
        ( cursor
          $/ X.element "tr" &/ X.element "td" &/ X.content
        )
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
                code <- toDecimal t
                Just (TSTYO code, T.strip c)
    --
    --
    price :: X.Cursor -> Maybe Double
    price cursor =
        let
            xs = cursor
                $/ X.element "tr" &/ X.element "td"
                &/ X.element "font" &/ X.content
        in
        case xs of
            [] -> Nothing
            _  -> toDouble . T.concat $ map T.strip xs
    --
    --
    diffCountGain :: X.Cursor -> Maybe (Maybe Double, Int32, Double)
    diffCountGain cursor =
        let
            xs = cursor
                $/ X.element "tr" &/ X.element "td"
                &/ X.element "table" &/ X.element "tr" &/ X.element "td"
            ys = filter (/="")
                . map T.strip
                . concatMap ($// X.content)
                . take 1 $ drop 2 xs
        in do
        d <- Maybe.listToMaybe . drop 1 $ ys
        c <- toDecimal =<< (Maybe.listToMaybe . drop 3 $ ys)
        g <- toDouble =<< (Maybe.listToMaybe . drop 5 $ ys)
        case d of
            "-" -> Just (Nothing,c,g)       -- 現在取引無し
            x   -> Just (toDouble x,c,g)
    --
    --
    pack :: X.Cursor -> Maybe HoldStockDetailPage
    pack cursor = do
        uid <- userid cursor
        (ts,ca) <- tickerCaption cursor
        pr <- price cursor
        (d,c,g) <- diffCountGain cursor
        Just HoldStockDetailPage
            { hsdUserid = uid
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
marketInfoPage html = do
    cap <- caption
    Just $ go cap
    where
    --
    --
    go :: T.Text -> MarketInfoPage
    go cap =
        \case
            Nothing ->  MarketInfoPage
                            { miCaption = cap
                            , miPrice = Nothing
                            , miMonth = Nothing
                            , miDay = Nothing
                            , miHour = Nothing
                            , miMinute = Nothing
                            , miDifference = Nothing
                            , miDiffPercent = Nothing
                            , miOpen = Nothing
                            , miHigh = Nothing
                            , miLow = Nothing
                            }
            Just x -> x
        $ do
            (pr,mo,da,ho,mi,df,dp) <- priceAtDiffPercent =<< Maybe.listToMaybe (take 1 $ drop 1 tables)
            (o,h,l) <- openHighLow =<< Maybe.listToMaybe (take 1 $ drop 2 tables)
            Just MarketInfoPage
                { miCaption = cap
                , miPrice = pr
                , miMonth = mo
                , miDay = da
                , miHour = ho
                , miMinute = mi
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
        Maybe.listToMaybe $
        root $// X.attributeIs "class" "titletext" &// X.content
    --
    --
    priceAtDiffPercent :: X.Cursor -> Maybe (Maybe Double, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Double, Maybe Double)
    priceAtDiffPercent cursor = do
        let tds = map T.strip $
                    cursor
                    $/ X.element "tr" &/ X.element "td" &// X.content
        -- 現在値
        pr <- Maybe.listToMaybe . take 1 . drop 1 $ tds
        -- 時間
        tm <- Maybe.listToMaybe . take 1 . drop 2 $ tds
        let tms = T.split (`elem` ['/',' ',':']) . T.init . T.tail $ T.strip tm
        M.guard (length tms == 4)
        let [month, day, hour, minute] = tms
        -- 前日比
        df <- Maybe.listToMaybe . take 1 . drop 4 $ tds
        -- 前日比(%)
        dp <- Maybe.listToMaybe . take 1 . drop 6 $ tds
        Just( toDouble pr
            , toDecimal month
            , toDecimal day
            , toDecimal hour
            , toDecimal minute
            , toDouble df
            , toDouble dp
            )
    --
    --
    openHighLow :: X.Cursor -> Maybe (Maybe Double, Maybe Double, Maybe Double)
    openHighLow cursor = do
        let tds = map T.strip $
                    cursor
                    $/ X.element "tr" &/ X.element "td" &// X.content
        -- 始値
        open <- Maybe.listToMaybe . take 1 . drop 1 $ tds
        -- 高値
        high <- Maybe.listToMaybe . take 1 . drop 3 $ tds
        -- 安値
        low <- Maybe.listToMaybe . take 1 . drop 5 $ tds
        Just (toDouble open, toDouble high, toDouble low)

