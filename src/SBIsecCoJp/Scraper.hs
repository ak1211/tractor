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
import           Control.Monad                    ((>=>))
import qualified Data.Char                        as C
import           Data.Int                         (Int32)
import qualified Data.Maybe                       as Mb
import           Data.Monoid                      (mempty, (<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import qualified Data.Text.Read                   as Read
import qualified Data.Time                        as Tm
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
    { miPrice       :: Double       -- ^ 現在値
    , miAt          :: Tm.UTCTime   -- ^ 時間
    , miDifference  :: Double       -- ^ 前日比
    , miDiffPercent :: Double       -- ^ 前日比(%)
    , miOpen        :: Double       -- ^ 始値
    , miHigh        :: Double       -- ^ 高値
    , miLow         :: Double       -- ^ 安値
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
        let
            frf = TLB.formatRealFloat TLB.Fixed (Just 2)
            ftm = TLB.fromString . show
        in
        TL.unpack . TLB.toLazyText $ mempty
        <> "現在値: " <> frf miPrice <> " " <> ftm miAt <> newline
        <> "前日比: " <> frf miDifference <> " " <> frf miDiffPercent <> "%" <> newline
        <> "open " <> frf miOpen <> newline
        <> "high " <> frf miHigh <> newline
        <> "low " <> frf miLow

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
    { hsdUserid        :: T.Text            -- ^ ユーザーID
    , hsdAt            :: Maybe Tm.UTCTime  -- ^ 時間(取引がない場合はNothing)
    , hsdTicker        :: TickerSymbol      -- ^ ティッカーシンボル
    , hsdCaption       :: T.Text            -- ^ 名前
    , hsdDiff          :: Maybe Double      -- ^ 前日比(取引がない場合はNothing)
    , hsdCount         :: Int32             -- ^ 保有数
    , hsdPurchasePrice :: Double            -- ^ 取得単価
    , hsdPrice         :: Double            -- ^ 現在値
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
        . Mb.listToMaybe
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
        Mb.listToMaybe . drop 1 $ map T.strip (cursor $/ X.content)
    --
    --
    pack :: X.Cursor -> Maybe PurchaseMarginDetailPage
    pack cursor =
        let xs = cursor
                $/ X.element "table" &/ X.element "tr" &/ X.element "td"
                &// X.content
        in do
        uid <- userid cursor
        day <- Mb.listToMaybe . drop 1 $ map T.strip xs
        mts <- toDecimal =<<  (Mb.listToMaybe . drop 3 $ map T.strip xs)
        cb <- toDecimal =<< (Mb.listToMaybe . drop 5 $ map T.strip xs)
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
        Mb.listToMaybe . drop 1 $ map T.strip (cursor $/ X.content)
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
        a <- Mb.listToMaybe $ drop 1 ys
        b <- Mb.listToMaybe $ drop 4 ys
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
                code <- toDecimal t
                Just (TSTYO code, T.strip c)
    --
    -- (現在値, 時間)
    priceAt :: X.Cursor -> Maybe (Double, Maybe Tm.UTCTime)
    priceAt cursor =
        let
            xs = filter (/= T.empty) . map T.strip $
                cursor
                $/ X.element "tr" &/ X.element "td"
                &// X.content
        in do
        -- 現在値
        pr <- toDouble =<< Mb.listToMaybe . drop 4 =<< pure xs
        -- 時間
        tm <- T.unpack . T.init . T.tail . T.dropWhile (/= '(')
                <$> (Mb.listToMaybe . drop 5 $ xs)
        Just( pr
            , Tm.parseTimeM True Tm.defaultTimeLocale "%m/%d %H:%M%z" $ tm ++ "+0900"
            )
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
        d <- \case{"-"->Nothing; x->toDouble x} <$> (Mb.listToMaybe . drop 1 $ ys)
        c <- toDecimal =<< Mb.listToMaybe . drop 3 =<< pure ys
        g <- toDouble =<< Mb.listToMaybe . drop 5 =<< pure ys
        Just (d, c, g)
    --
    --
    pack :: X.Cursor -> Maybe HoldStockDetailPage
    pack cursor = do
        uid <- userid cursor
        (ts,ca) <- tickerCaption cursor
        (pr,at) <- priceAt cursor
        (d,c,g) <- diffCountGain cursor
        Just HoldStockDetailPage
            { hsdUserid = uid
            , hsdAt = at
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
        (pr,at,df,dp) <- priceAtDiffPercent =<< Mb.listToMaybe (take 1 $ drop 1 tables)
        (o,h,l) <- openHighLow =<< Mb.listToMaybe (take 1 $ drop 2 tables)
        Just MarketInfo
            { miPrice = pr
            , miAt = at
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
    priceAtDiffPercent :: X.Cursor -> Maybe (Double, Tm.UTCTime, Double, Double)
    priceAtDiffPercent cursor = do
        let tds = map T.strip $
                    cursor
                    $/ X.element "tr" &/ X.element "td" &// X.content
        -- 現在値
        pr <- toDouble =<< Mb.listToMaybe . take 1 . drop 1 =<< pure tds
        -- 時間
        tm <- T.unpack . T.init . T.tail . T.strip <$> (Mb.listToMaybe . drop 2 $ tds)
        at <- Tm.parseTimeM True Tm.defaultTimeLocale "%m/%d %H:%M%z" $ tm ++ "+0900"
        -- 前日比
        df <- toDouble =<< Mb.listToMaybe . drop 4 =<< pure tds
        -- 前日比(%)
        dp <- toDouble =<< Mb.listToMaybe . drop 6 =<< pure tds
        Just (pr, at, df, dp)
    --
    --
    openHighLow :: X.Cursor -> Maybe (Double, Double, Double)
    openHighLow cursor = do
        let tds = map T.strip $
                    cursor
                    $/ X.element "tr" &/ X.element "td" &// X.content
        -- 始値
        open <- toDouble =<< Mb.listToMaybe . drop 1 =<< pure tds
        -- 高値
        high <- toDouble =<< Mb.listToMaybe . drop 3 =<< pure tds
        -- 安値
        low <- toDouble =<< Mb.listToMaybe . drop 5 =<< pure tds
        Just (open, high, low)

