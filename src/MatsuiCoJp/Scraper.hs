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
    ( FraAstSpare (..)
    , FraHomeAnnounce (..)
    , FraStkSell (..)
    , HoldStock (..)
    , nonScraping
    , OrderConfirmed (..)
    , scrapingFraAstSpare
    , scrapingFraHomeAnnounce
    , scrapingFraStkSell
    , scrapingOrderConfirmed
    ) where

import           Control.Monad       ((>=>))
import qualified Control.Monad       as M
import qualified Data.Char
import           Data.Int            (Int32)
import qualified Data.List           as List
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.Read as Read
import qualified Safe
import qualified Text.HTML.DOM       as H
import           Text.XML.Cursor     (($/), ($//), (&/), (&//), (&|))
import qualified Text.XML.Cursor     as X

-- |
-- ホーム -> お知らせの内容
data FraHomeAnnounce = FraHomeAnnounce
    { fsAnnounceDeriverTime   :: TL.Text    -- ^ お知らせの配信時間
    , fsAnnounceLastLoginTime :: TL.Text    -- ^ 前回ログイン
    , fsAnnounces             :: [TL.Text]  -- ^ お知らせ
    } deriving (Eq, Show)

-- |
-- 保有株(個別銘柄)情報
data HoldStock = HoldStock
    { hsSellOrderUrl  :: Maybe TL.Text  -- ^ 売り注文ページurl
    , hsCode          :: Int            -- ^ 証券コード
    , hsCaption       :: TL.Text        -- ^ 名前
    , hsCount         :: Int            -- ^ 保有数
    , hsPurchasePrice :: Double         -- ^ 取得単価
    , hsPrice         :: Double         -- ^ 現在値
    } deriving (Eq, Show)

-- |
--  株式取引 -> 現物売の内容
data FraStkSell = FraStkSell
    { fsEvaluation  :: Double      -- ^ 評価合計
    , fsProfit      :: Double      -- ^ 損益合計
    , fsStocks      :: [HoldStock] -- ^ 個別銘柄情報
    } deriving (Eq, Show)

-- |
-- 資産状況 -> 余力情報の内容
data FraAstSpare = FraAstSpare
    { faMoneySpare  :: Int32     -- ^ 現物買付余力
    , faCashBalance :: Int32     -- ^ 現金残高
    , faDepositInc  :: Int32     -- ^ 預り増加額
    , faDepositDec  :: Int32     -- ^ 預り減少額
    , faBindingFee  :: Int32     -- ^ ボックスレート手数料拘束金
    , faBindingTax  :: Int32     -- ^ 源泉徴収税拘束金（仮計算）
    , faFreeCash    :: Int32     -- ^ 使用可能現金
    } deriving (Eq, Show)

-- |
-- 注文発注後の"ご注文を受付けました"の内容
data OrderConfirmed = OrderConfirmed
    { contents            :: TL.Text
    } deriving (Eq, Show)

-- |
-- 符号あるいは数字または小数点か判定する関数
isSignDigit :: Char -> Bool
isSignDigit c = List.any ($ c) [Data.Char.isDigit, (=='+'), (=='-'), (=='.')]

-- |
--  符号,数字,小数点以外の文字を破棄する関数
onlySignDigit :: TL.Text -> TL.Text
onlySignDigit = TL.filter isSignDigit

--
--
doubleFromTxt :: TL.Text -> Either TL.Text Double
doubleFromTxt t =
    either (const $ Left $ TL.concat ["文字から数字への変換に失敗:\"", t, "\""]) Right
    (fst <$> (Read.signed Read.double $ onlySignDigit t))

--
--
decimalFromTxt :: Integral a => TL.Text -> Either TL.Text a
decimalFromTxt t =
    either (const $ Left $ TL.concat ["文字から数字への変換に失敗:\"", t, "\""]) Right
    (fst <$> (Read.signed Read.decimal $ onlySignDigit t))

-- |
-- そのままページを返す関数
nonScraping :: [TL.Text] -> Either TL.Text TL.Text
nonScraping []    = Left "ページを受け取れていません。"
nonScraping (h:_) = Right h


-- |
-- "ホーム" -> "お知らせ" のページをスクレイピングする関数
scrapingFraHomeAnnounce :: [TL.Text] -> Either TL.Text FraHomeAnnounce
scrapingFraHomeAnnounce [] = Left "お知らせページを受け取れていません。"
scrapingFraHomeAnnounce (html:_) = do
    --
    -- お知らせには次のページが無いので先頭ページのみが対象
    --
    at <- deriverAt
    ll <- lastLogin
    Right FraHomeAnnounce
        { fsAnnounceDeriverTime   = at
        , fsAnnounceLastLoginTime = ll
        , fsAnnounces             = filter (not . TL.null)
                                    $ map (TL.dropAround (== '\n')) announces
        }
    where
    --
    --
    root = X.fromDocument $ H.parseLT html
    --
    -- <B></B>で囲まれた要素
    bolds :: [TL.Text]
    bolds = map TL.fromStrict $
        root $// X.element "B" &/ X.content
    --
    -- 配信時間 (<B></B>で囲まれた要素の2番目)
    deriverAt :: Either TL.Text TL.Text
    deriverAt =
        Safe.headDef (Left "お知らせ配信時間の取得に失敗")
            . map Right . drop 1 $ bolds
    --
    -- 前回ログイン時間 (<B></B>で囲まれた要素の3番目)
    lastLogin :: Either TL.Text TL.Text
    lastLogin =
        Safe.headDef (Left "前回ログイン時間の取得に失敗")
            . map Right . drop 2 $ bolds
    --
    -- おしらせの内容
    announces :: [TL.Text]
    announces = map TL.fromStrict $
        root $// X.element "TABLE" >=> X.attributeIs "cellspacing" "5" &// X.content

-- |
--  "株式取引" -> "現物売" のページをスクレイピングする関数
scrapingFraStkSell :: [TL.Text] -> Either TL.Text FraStkSell
scrapingFraStkSell [] = Left "現物売ページを受け取れていません。"
scrapingFraStkSell (html:_) = do
    --
    -- TODO: 現物売ページの次のページを確認したことが無いのでいまは後続ページを無視する
    --
    profit <- sumProfit . take 1 . drop 4 $ tblSummary
    ev <- evaluation . take 1 . drop 6 $ tblSummary
    case emptyCondition myHoldStocks of
        True ->
            Right FraStkSell    { fsEvaluation = ev
                                , fsProfit = profit
                                , fsStocks = []
                                }
        False -> do
            stocks <- M.mapM takeHoldStock myHoldStocks
            Right FraStkSell    { fsEvaluation = ev
                                , fsProfit = profit
                                , fsStocks = stocks
                                }
    where
    --
    --
    root = X.fromDocument $ H.parseLT html
    --
    -- 株式評価損益, 株式時価評価額が書いてあるテーブル
    tblSummary :: [TL.Text]
    tblSummary =
        map TL.fromStrict $ root
        $// X.element "TABLE" >=> X.attributeIs "border" "1"
        &/ X.element "TBODY"
        &/ X.element "TR"
        &/ X.element "TD"
        &// X.content
    -- |
    -- 株式評価損益を取り出す
    sumProfit :: [TL.Text] -> Either TL.Text Double
    sumProfit [t] = doubleFromTxt t
    sumProfit _   = Left "株式評価損益の取得に失敗"
    --
    -- 株式時価評価額を取り出す
    evaluation :: [TL.Text] -> Either TL.Text Double
    evaluation [t] = doubleFromTxt t
    evaluation _   = Left "株式時価評価額の取得に失敗"
    --
    -- 保有株式が書いてあるテーブルのリスト
    myHoldStocks :: [[(TL.Text, TL.Text, TL.Text)]]
    myHoldStocks =
        root
        $// X.element "TABLE" >=> X.attributeIs "border" "1"
        &/ X.element "TR" >=> X.followingSibling >=> X.anyElement
        &| tr
        where
        --
        -- 保有株式テーブル内の"TR"要素
        tr :: X.Cursor -> [(TL.Text, TL.Text, TL.Text)]
        tr c = c $/ X.element "TD" &| td
        --
        -- "TR"要素内の"TD"要素
        td :: X.Cursor -> (TL.Text, TL.Text, TL.Text)
        td c =
            (myTextLeader, myTextTrailer, myHref)
            where
            --
            -- "TD"要素内の内容を結合したテキストで"<BR>"等の前と後
            myTextLeader = TL.concat . take 1 . drop 0 $ myTexts
            myTextTrailer = TL.concat . take 1 . drop 1 $ myTexts
            myTexts = map TL.fromStrict (c $// X.content)
            --
            -- "TD"要素内の"href"属性を結合したテキスト
            myHref = TL.concat . take 1 $ map TL.fromStrict (c $// X.attribute "href")
    --
    -- 保有株式テーブルの内容が
    -- <B>株式残高はありません。</B>
    -- であることを確認する
    emptyCondition :: [[(TL.Text, TL.Text, TL.Text)]] -> Bool
    emptyCondition ((stock:_):_) =
        first3 stock =="株式残高はありません。"
    emptyCondition _ = False
    --
    -- 保有株式リストから株式情報を得る
    takeHoldStock :: [(TL.Text, TL.Text, TL.Text)] -> Either TL.Text HoldStock
    takeHoldStock = go
        where
        --
        -- 保有株式テーブルの内容が
        -- 売 特定 新華ホールディングス・リミテッド[東]9399 3 189 1870 5610 -6-1.05% 0
        -- こういうリストであるときに、内容を受け取って分解する
        go (url : _ : captioncode : count : purchase : price : _) = do
            let url' = (\x -> if x == "" then Nothing else Just x) $ (third3 url)
            --
            -- HACK: 東証以外は考えていない
            --
            let (_, code) = TL.breakOn "[東]" $ scond3 captioncode
            code'       <- decimalFromTxt code
            count'      <- decimalFromTxt (first3 count)
            purchase'   <- doubleFromTxt (first3 purchase)
            price'      <- doubleFromTxt (first3 price)
            Right HoldStock
                { hsSellOrderUrl  = url'
                , hsCode          = code'
                , hsCaption       = first3 captioncode
                , hsCount         = count'
                , hsPurchasePrice = purchase'
                , hsPrice         = price'
                }
        go _ = Left "保有株式の取得に失敗"
    --
    --
    first3 (x,_,_) = x
    scond3 (_,y,_) = y
    third3 (_,_,z) = z

-- |
-- 資産状況 -> 余力情報のページをスクレイピングする関数
scrapingFraAstSpare :: [TL.Text] -> Either TL.Text FraAstSpare
scrapingFraAstSpare [] = Left "余力情報ページを受け取れていません。"
scrapingFraAstSpare (html:_) = do
    --
    -- 余力情報には次のページが無いので先頭ページのみ
    --
    spare   <- moneySpare   . take 1 . drop 0 $ mySpares
    -- 出金余力(1日後)は無視する
    -- 出金余力(2日後)は無視する
    -- 出金余力(3日後)は無視する
    -- 出金余力(4日後)は無視する
    balance <- cashBalance  . take 1 . drop 5 $ mySpares
    inc     <- depositInc   . take 1 . drop 6 $ mySpares
    dec     <- depositDec   . take 1 . drop 7 $ mySpares
    fee     <- bindingFee   . take 1 . drop 8 $ mySpares
    tax     <- bindingTax   . take 1 . drop 9 $ mySpares
    cash    <- freeCash     . take 1 . drop 10 $ mySpares
    Right FraAstSpare
        { faMoneySpare = spare
        , faCashBalance = balance
        , faDepositInc = inc
        , faDepositDec = dec
        , faBindingFee = fee
        , faBindingTax = tax
        , faFreeCash = cash
        }
    where
    --
    --
    root = X.fromDocument $ H.parseLT html
    -- |
    -- 現物買付余力を取り出す
    moneySpare :: Integral a => [TL.Text] -> Either TL.Text a
    moneySpare [t] = decimalFromTxt t
    moneySpare _   = Left "現物買付余力の取得に失敗"
    -- |
    -- 現金残高を取り出す
    cashBalance :: Integral a => [TL.Text] -> Either TL.Text a
    cashBalance [t] = decimalFromTxt t
    cashBalance _   = Left "現金残高の取得に失敗"
    -- |
    -- 預り増加額を取り出す
    depositInc :: Integral a => [TL.Text] -> Either TL.Text a
    depositInc [t] = decimalFromTxt t
    depositInc _   = Left "預り増加額の取得に失敗"
    -- |
    -- 預り減少額を取り出す
    depositDec :: Integral a => [TL.Text] -> Either TL.Text a
    depositDec [t] = decimalFromTxt t
    depositDec _   = Left "預り減少額の取得に失敗"
    -- |
    -- ボックスレート手数料拘束金を取り出す
    bindingFee :: Integral a => [TL.Text] -> Either TL.Text a
    bindingFee [t] = decimalFromTxt t
    bindingFee _   = Left "ボックスレート手数料拘束金の取得に失敗"
    -- |
    -- 源泉徴収税拘束金（仮計算）を取り出す
    bindingTax :: Integral a => [TL.Text] -> Either TL.Text a
    bindingTax [t] = decimalFromTxt t
    bindingTax _   = Left "源泉徴収税拘束金（仮計算）の取得に失敗"
    -- |
    -- 使用可能現金を取り出す
    freeCash :: Integral a => [TL.Text] -> Either TL.Text a
    freeCash [t] = decimalFromTxt t
    freeCash _   = Left "使用可能現金の取得に失敗"
    --
    -- 余力情報リスト
    mySpares :: [TL.Text]
    mySpares =
        map TL.fromStrict $ root
        $// X.element "TR"
        &/ X.element "TD" >=> X.attributeIs "align" "right"
        &/ X.content

-- |
--  "ご注文を受け付けました"のページをスクレイピングする関数
scrapingOrderConfirmed :: [TL.Text] -> Either TL.Text OrderConfirmed
scrapingOrderConfirmed [] = Left "注文終了ページを受け取れていません。"
scrapingOrderConfirmed (html:_) = do
    --
    -- 注文終了ページには次のページが無いので先頭ページのみ
    --
    Right OrderConfirmed { contents = html }

