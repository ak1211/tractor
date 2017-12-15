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
Module      :  MatsuiCoJp.Scraper
Description :  Scraping a web page
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

スクレイピングするモジュールです。
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module MatsuiCoJp.Scraper
    ( taglist
    , tag
    , table
    , tr
    , td
    , textlist
    , text
    , href
    , HoldStock (..)
    , FraHomeAnnounce (..)
    , FraStkSell (..)
    , FraAstSpare (..)
    , OrderConfirmed (..)
    , nonScraping
    , scrapingFraHomeAnnounce
    , scrapingFraStkSell
    , scrapingFraAstSpare
    , scrapingOrderConfirmed
    ) where

import qualified Control.Monad          as M
import qualified Data.Char
import           Data.Int               (Int32)
import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Read    as Read
import qualified Safe
import qualified Text.HTML.TagSoup      as TS
import qualified Text.HTML.TagSoup.Tree as TS

-- |
-- ホーム -> お知らせの内容
data FraHomeAnnounce = FraHomeAnnounce
    { fsAnnounceDeriverTime   :: TL.Text    -- ^ お知らせの配信時間
    , fsAnnounceLastLoginTime :: TL.Text    -- ^ 前回ログイン
    , fsAnnounces             :: [TL.Text]  -- ^ お知らせ
    } deriving (Eq)

-- |
-- 保有株(個別銘柄)情報
data HoldStock = HoldStock
    { hsSellOrderUrl  :: Maybe TL.Text  -- ^ 売り注文ページurl
    , hsCode          :: Int            -- ^ 証券コード
    , hsCaption       :: TL.Text        -- ^ 名前
    , hsCount         :: Int            -- ^ 保有数
    , hsPurchasePrice :: Double         -- ^ 取得単価
    , hsPrice         :: Double         -- ^ 現在値
    } deriving (Eq)

-- |
--  株式取引 -> 現物売の内容
data FraStkSell = FraStkSell
    { fsQuantity :: Double      -- ^ 評価合計
    , fsProfit   :: Double      -- ^ 損益合計
    , fsStocks   :: [HoldStock] -- ^ 個別銘柄情報
    } deriving (Eq)

-- |
-- 資産状況 -> 余力情報の内容
data FraAstSpare = FraAstSpare
    { faMoneyToSpare       :: Int32     -- ^ 現物買付余力
    , faStockOfMoney       :: Int32     -- ^ 現金残高
    , faIncreaseOfDeposits :: Int32     -- ^ 預り増加額
    , faDecreaseOfDeposits :: Int32     -- ^ 預り減少額
    , faRestraintFee       :: Int32     -- ^ ボックスレート手数料拘束金
    , faRestraintTax       :: Int32     -- ^ 源泉徴収税拘束金（仮計算）
    , faCash               :: Int32     -- ^ 使用可能現金
    } deriving (Eq)

-- |
-- 注文発注後の"ご注文を受付けました"の内容
data OrderConfirmed = OrderConfirmed
    { contents            :: TL.Text
    } deriving (Eq)

-- |
-- タグ名の子をリストで取り出す関数
taglist :: TL.Text -> [TS.TagTree TL.Text] -> [[TS.TagTree TL.Text]]
taglist nm t = [c | TS.TagBranch k _ c <- TS.universeTree t, nm==TL.toLower k]

-- |
-- タグ名の子リストからidx番のタグを取り出す関数
tag :: TL.Text -> Int -> [TS.TagTree TL.Text] -> Maybe [TS.TagTree TL.Text]
tag name idx = flip Safe.atMay idx . taglist name

--
--
table :: Int -> [TS.TagTree TL.Text] -> Maybe [TS.TagTree TL.Text]
table = tag "table"

--
--
tr :: Int -> [TS.TagTree TL.Text] -> Maybe [TS.TagTree TL.Text]
tr = tag "tr"

--
--
td :: Int -> [TS.TagTree TL.Text] -> Maybe [TS.TagTree TL.Text]
td = tag "td"

-- |
-- テキスト要素をリストで取り出す関数
textlist :: [TS.TagTree TL.Text] -> [TL.Text]
textlist t =
    filter (/="") [TL.strip txt | TS.TagText txt <- TS.flattenTree t]

-- |
-- idx番のテキストを取り出す関数
text :: Int -> [TS.TagTree TL.Text] -> Maybe TL.Text
text idx t =
    Safe.atMay [TL.strip txt | TS.TagText txt <- TS.flattenTree t] idx

-- |
-- aタグからhref属性を取り出す関数
href :: Int -> [TS.TagTree TL.Text] -> Maybe TL.Text
href nm t =
    Safe.atMay [as | TS.TagBranch k as _ <- TS.universeTree t, "a"==TL.toLower k] nm
    >>= \as -> Maybe.listToMaybe [v | (k, v) <- as, "href"==TL.toLower k]

-- |
-- 符号あるいは数字または小数点か判定する関数
isSignDigit :: Char -> Bool
isSignDigit c = List.any ($ c) [Data.Char.isDigit, (=='+'), (=='-'), (=='.')]

-- |
--  符号,数字,小数点以外の文字を破棄する関数
onlySignDigit :: TL.Text -> TL.Text
onlySignDigit = TL.filter isSignDigit

--
--  Textからそれぞれの型に変換する関数
toText :: Maybe TL.Text -> TL.Text -> Either TL.Text TL.Text
toText Nothing note = Left note
toText (Just t) _   = Right t

--
--
toDecimal :: Integral a => Maybe TL.Text -> TL.Text -> Either TL.Text a
toDecimal Nothing note = Left note
toDecimal (Just t) note =
    case Read.signed Read.decimal $ onlySignDigit t of
        Right (v, _) -> Right v
        Left _       -> Left note

--
--
toDouble :: Maybe TL.Text -> TL.Text -> Either TL.Text Double
toDouble Nothing note = Left note
toDouble (Just t) note =
    case Read.signed Read.double $ onlySignDigit t of
        Right (v, _) -> Right v
        Left _       -> Left note

-- |
-- そのままページを返す関数
nonScraping :: [TL.Text] -> Either TL.Text TL.Text
nonScraping []    = Left "ページを受け取れていません。"
nonScraping (h:_) = Right h


-- |
-- "ホーム" -> "お知らせ" のページをスクレイピングする関数
scrapingFraHomeAnnounce :: [TL.Text] -> Either TL.Text FraHomeAnnounce
scrapingFraHomeAnnounce htmls = do
    html <- case htmls of
        []  -> Left "お知らせページを受け取れていません。"
        -- お知らせには次のページが無いので先頭のみを取り出す
        x:_ -> Right x
    let tree = TS.tagTree $ TS.parseTags html
    deriverTime     <- (Just tree >>= table 0 >>= tr 0 >>= table 0 >>= td 1 >>= text 0) `toText` "お知らせ配信時間の取得に失敗"
    lastLoginTime   <- (Just tree >>= table 0 >>= tr 6 >>= td 0 >>= text 0) `toText` "前回ログイン時間の取得に失敗"

    -- お知らせ
    let announces = textlist <$> (Just tree >>= table 0 >>= tr 8 >>= td 1)
    -- 返値
    Right FraHomeAnnounce {
        fsAnnounceDeriverTime   = deriverTime,
        fsAnnounceLastLoginTime = lastLoginTime,
        fsAnnounces             = Maybe.fromMaybe [] announces
    }

-- |
--  "株式取引" -> "現物売" のページをスクレイピングする関数
scrapingFraStkSell :: [TL.Text] -> Either TL.Text FraStkSell
scrapingFraStkSell htmls = do
    html <- case htmls of
        []  -> Left "現物売ページを受け取れていません。"
        {-
            現物売ページの次のページを確認したことが無いのでいまは後続ページを無視する
        -}
        x:_ -> Right x
    let tree = TS.tagTree $ TS.parseTags html
    -- 株式評価損益を取り出す
    sumProfit <- (Just tree >>= table 2 >>= tr 2 >>= td 1 >>= text 0) `toDouble` "株式評価損益の取得に失敗"
    -- 株式時価評価額を取り出す
    sumQuantity <- (Just tree >>= table 2 >>= tr 2 >>= td 2 >>= text 0) `toDouble` "株式時価評価額の取得に失敗"
    -- 銘柄リストを取り出す
    lists <- maybe
            (Left "個別株式リストの取得に失敗")
            (Right . drop 1 . taglist "tr")
            (Just tree >>= table 8)
    -- 銘柄リストから株式情報を得る
    stocks <- M.mapM takeHoldStock lists

    -- 返値
    Right FraStkSell {
        fsQuantity = sumQuantity,
        fsProfit = sumProfit,
        fsStocks = stocks
    }
    where
    -- 銘柄リストから株式情報を得る
    takeHoldStock :: [TS.TagTree TL.Text] -> Either TL.Text HoldStock
    takeHoldStock tree = do
        url     <- (Just tree >>= td 0 >>= href 0) `toText`     "売り注文ページurlの取得に失敗"
        caption <- (Just tree >>= td 2 >>= text 0) `toText`     "銘柄名の取得に失敗"
        code    <- (Just tree >>= td 2 >>= text 1) `toDecimal`  "証券コードの取得に失敗"
        count   <- (Just tree >>= td 3 >>= text 0) `toDecimal`  "保有数の取得に失敗"
        purchase<- (Just tree >>= td 4 >>= text 0) `toDouble`   "取得単価の取得に失敗"
        price   <- (Just tree >>= td 5 >>= text 0) `toDouble`   "現在値の取得に失敗"

        Right HoldStock {
            hsSellOrderUrl  = Just url,
            hsCode          = code,
            hsCaption       = caption,
            hsCount         = count,
            hsPurchasePrice = purchase,
            hsPrice         = price
        }

-- |
-- 資産状況 -> 余力情報のページをスクレイピングする関数
scrapingFraAstSpare :: [TL.Text] -> Either TL.Text FraAstSpare
scrapingFraAstSpare htmls = do
    html <- case htmls of
        []  -> Left "余力情報ページを受け取れていません。"
        -- 余力情報には次のページが無いので先頭のみを取り出す
        x:_ -> Right x
    let tree = TS.tagTree $ TS.parseTags html
    let node = Just tree >>= table 5
    mts <- (node >>= table 0          >>= td 1 >>= text 0) `toDecimal` "現物買付余力の取得に失敗"
    som <- (node >>= table 1 >>= tr 1 >>= td 1 >>= text 0) `toDecimal` "現金残高の取得に失敗"
    inc <- (node >>= table 1 >>= tr 2 >>= td 1 >>= text 0) `toDecimal` "預り増加額の取得に失敗"
    dec <- (node >>= table 1 >>= tr 3 >>= td 1 >>= text 0) `toDecimal` "預り減少額の取得に失敗"
    rfe <- (node >>= table 1 >>= tr 4 >>= td 1 >>= text 0) `toDecimal` "ボックスレート手数料拘束金の取得に失敗"
    rta <- (node >>= table 1 >>= tr 5 >>= td 1 >>= text 0) `toDecimal` "源泉徴収税拘束金（仮計算）の取得に失敗"
    cas <- (node >>= table 1 >>= tr 6 >>= td 1 >>= text 0) `toDecimal` "使用可能現金の取得に失敗"
    -- 返値
    Right FraAstSpare {
        faMoneyToSpare = mts,
        faStockOfMoney = som,
        faIncreaseOfDeposits = inc,
        faDecreaseOfDeposits = dec,
        faRestraintFee = rfe,
        faRestraintTax = rta,
        faCash = cas
    }

-- |
--  "ご注文を受け付けました"のページをスクレイピングする関数
scrapingOrderConfirmed :: [TL.Text] -> Either TL.Text OrderConfirmed
scrapingOrderConfirmed htmls = do
    html <- case htmls of
        []  -> Left "注文終了ページを受け取れていません。"
        -- 注文終了ページには次のページが無いので先頭のみを取り出す
        x:_ -> Right x
    -- 返値
    Right OrderConfirmed {
        contents = html
    }

