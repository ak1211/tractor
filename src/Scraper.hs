{- |
Module      :  Scraper.hs
Description :  Scraping a web page
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  portable

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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Scraper
    ( taglist
    , tag
    , table
    , tr
    , td
    , textlist
    , text
    , href
    , HoldStock (..)
    , hsGain
    , FraHomeAnnounce (..)
    , FraStkSell (..)
    , FraAstSpare (..)
    , Contents (..)
    , scrapingFraHomeAnnounce
    , scrapingFraStkSell
    , scrapingFraAstSpare
    ) where

import qualified Conf

import Debug.Trace (trace)
import Prelude hiding (catch)
import Control.Exception
import Safe

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Reader as M
import Control.Monad.Trans.Resource (runResourceT)

import Data.Conduit (Source, ($$), yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Data.Int (Int64)
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as List.Split
import qualified Data.Char
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Lazy.Read as Read
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Typeable
import qualified Data.Maybe

import qualified Codec.Text.IConv as IConv
import qualified Text.Printf as Printf

import qualified Network.Connection as N
import qualified Network.URI as N
import qualified Network.HTTP.Conduit as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Method as N

import Database.Persist ((==.), (!=.), (>.), (<=.))
import qualified Database.Persist as DB
import qualified Database.Persist.TH as DB
import qualified Database.Persist.Sql as DB
import qualified Database.Persist.Sqlite as Sqlite

import Text.HTML.TagSoup ((~==), (~/=))
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Tree as TS
import qualified Text.HTML.TagSoup.Match as TS

import Data.Time (Day(..), TimeOfDay, UTCTime, parseTime, getCurrentTime)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar, putMVar, readMVar)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)

{-
 - ページからスクレイピングした情報の型クラス
 -}
class Contents a where
    storeToDB :: UTCTime -> a -> IO ()

{-
 - ホーム -> お知らせの内容
 -}
data FraHomeAnnounce = FraHomeAnnounce {
    fsAnnounceDeriverTime   :: T.Text,  -- お知らせの配信時間
    fsAnnounceLastLoginTime :: T.Text,  -- 前回ログイン
    fsAnnounces             :: [T.Text] -- お知らせ
} deriving (Eq)

-- 型クラスShowのインスタンス
instance Show FraHomeAnnounce where
    show (FraHomeAnnounce dt lt as) =
        T.unpack $ T.unlines (dt : lt : as)

{-
 - 保有株(個別銘柄)情報
 -}
data HoldStock = HoldStock {
    hsSellOrderUrl  :: Maybe T.Text,    -- 売り注文ページurl
    hsCode          :: Int,             -- 証券コード
    hsCaption       :: T.Text,          -- 名前
    hsCount         :: Int,             -- 保有数
    hsPurchasePrice :: Double,          -- 取得単価
    hsPrice         :: Double           -- 現在値
} deriving (Eq)

-- 損益は計算で求める
hsGain (HoldStock sol cod cap cou pup pri) = realToFrac cou * (pri - pup)

-- 型クラスShowのインスタンス
instance Show HoldStock where
    show hs =
       let (HoldStock sol cod cap cou pup pri) = hs in
       Printf.printf "%d %s, 保有 %d, 取得 %f, 現在 %f, 損益 %+f"
           cod cap cou pup pri (hsGain hs)

{-
 - 株式取引 -> 現物売の内容
 -}
data FraStkSell = FraStkSell {
    fsQuantity  :: Double,      -- 評価合計
    fsProfit    :: Double,      -- 損益合計
    fsStocks    :: [HoldStock]  -- 個別銘柄情報
 } deriving (Eq)

-- 型クラスShowのインスタンス
instance Show FraStkSell where
    show (FraStkSell qua pro stk) =
        unlines [
           Printf.printf "評価合計 %f, 損益合計 %+f" qua pro,
           unlines $ map show stk]

{-
 - 資産状況 -> 余力情報の内容
 -}
data FraAstSpare = FraAstSpare {
    faMoneyToSpare          :: Int64,     -- 現物買付余力
    faStockOfMoney          :: Int64,     -- 現金残高
    faIncreaseOfDeposits    :: Int64,     -- 預り増加額
    faDecreaseOfDeposits    :: Int64,     -- 預り減少額
    faRestraintFee          :: Int64,     -- ボックスレート手数料拘束金
    faRestraintTax          :: Int64,     -- 源泉徴収税拘束金（仮計算）
    faCash                  :: Int64      -- 使用可能現金
} deriving (Eq)

-- 型クラスShowのインスタンス
instance Show FraAstSpare where
    show (FraAstSpare mts som inc dec rfe rta cas) =
        Printf.printf "現物買付余力 %d, " mts
        ++ Printf.printf "現金残高 %d, " som
        ++ Printf.printf "預り増加額 %d, " inc
        ++ Printf.printf "預り減少額 %d, " dec
        ++ Printf.printf "ボックスレート手数料拘束金 %d, " rfe
        ++ Printf.printf "源泉徴収税拘束金（仮計算） %d, " rta
        ++ Printf.printf "使用可能現金 %d" cas

{-
 - nmタグの子タグのリストを取り出す関数
 -}
taglist :: T.Text -> [TS.TagTree T.Text] -> [[TS.TagTree T.Text]]
taglist nm t = [c | TS.TagBranch k _ c <- TS.universeTree t, nm==T.toLower k]

{-
 - nameタグのidx番の子タグを取り出す関数
 -}
tag :: T.Text -> Int -> [TS.TagTree T.Text] -> Maybe [TS.TagTree T.Text]
tag name idx = flip Safe.atMay idx . taglist name

table = tag "table"
tr = tag "tr"
td = tag "td"

{-
 - idx番のテキストを取り出す関数
 -}
textlist :: [TS.TagTree T.Text] -> [T.Text]
textlist t =
    filter (/="") $ [T.strip txt | TS.TagText txt <- TS.flattenTree t]

{-
 - idx番のテキストを取り出す関数
 -}
text :: Int -> [TS.TagTree T.Text] -> Maybe T.Text
text idx t =
    Safe.atMay [T.strip txt | TS.TagText txt <- TS.flattenTree t] idx

{-
 - aタグからhref属性を取り出す関数
 -}
href :: Int -> [TS.TagTree T.Text] -> Maybe T.Text
href nm t =
    Safe.atMay [as | TS.TagBranch k as _ <- TS.universeTree t, "a"==T.toLower k] nm
    >>= \as -> Maybe.listToMaybe [v | (k, v) <- as, "href"==T.toLower k]

{-
 - 符号あるいは数字または小数点か判定する関数
 -}
isSignDigit :: Char -> Bool
isSignDigit c = List.any ($ c) [Data.Char.isDigit, (=='+'), (=='-'), (=='.')]

{-
 - 符号,数字,小数点以外の文字を破棄する関数
 -}
onlySignDigit :: T.Text -> T.Text
onlySignDigit = T.filter isSignDigit

{-
 - Textからそれぞれの型に変換する関数
 -}
toText :: Maybe T.Text -> T.Text -> Either T.Text T.Text
toText Nothing note = Left note
toText (Just t) _ = Right t

toDecimal :: Integral a => Maybe T.Text -> T.Text -> Either T.Text a
toDecimal Nothing note = Left note
toDecimal (Just t) note =
    case Read.signed Read.decimal $ onlySignDigit t of
        Right (v, _) -> Right v
        Left _ -> Left note

toDouble :: Maybe T.Text -> T.Text -> Either T.Text Double
toDouble Nothing note = Left note
toDouble (Just t) note =
    case Read.signed Read.double $ onlySignDigit t of
        Right (v, _) -> Right v
        Left _ -> Left note

{-
 - "ホーム" -> "お知らせ" のページをスクレイピングする関数
 -}
scrapingFraHomeAnnounce :: [T.Text] -> Either T.Text FraHomeAnnounce
scrapingFraHomeAnnounce htmls = do
    html <- case htmls of
        []  -> Left "スクレイピング対象のページがありません。"
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

{-
 - "株式取引" -> "現物売" のページをスクレイピングする関数
 -}
scrapingFraStkSell :: [T.Text] -> Either T.Text FraStkSell
scrapingFraStkSell htmls = do
    html <- case htmls of
        []  -> Left "スクレイピング対象のページがありません。"
        {-
         - 現物売ページの次のページを確認したことが無いのでいまは後続ページを無視する
         -}
        x:_ -> Right x
    let tree = TS.tagTree $ TS.parseTags html
    -- 株式評価損益を取り出す
    sumProfit <- (Just tree >>= table 2 >>= tr 2 >>= td 1 >>= text 0) `toDouble` "株式評価損益の取得に失敗"
    -- 株式時価評価額を取り出す
    sumQuantity <- (Just tree >>= table 2 >>= tr 2 >>= td 2 >>= text 0) `toDouble` "株式時価評価額の取得に失敗"
    -- 銘柄リストを取り出す
    lists <- Maybe.maybe
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
    takeHoldStock :: [TS.TagTree T.Text] -> Either T.Text HoldStock
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

{-
 - 資産状況 -> 余力情報のページをスクレイピングする関数
 -}
scrapingFraAstSpare :: [T.Text] -> Either T.Text FraAstSpare
scrapingFraAstSpare htmls = do
    html <- case htmls of
        []  -> Left "スクレイピング対象のページがありません。"
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

