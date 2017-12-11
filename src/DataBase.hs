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
Module      :  DataBase
Description :  store to database of the securities information
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

データベースとのやりとりをするモジュールです。
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module DataBase
    ( Summary (..)
    , HoldStock (..)
    , AssetSpare (..)
    , TotalAssets (..)
    , getTotalAstsDescList
    , totalAssetsOfCash
    , getHoldStockDescList
    , storeStkSell
    , storeAssetSpare
    ) where

import qualified Control.Monad           as M
import           Data.Int
import qualified Data.Text.Lazy          as T
import           Data.Time               (UTCTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import qualified Database.Persist.Sqlite as DB
import qualified Database.Persist.TH     as DB

import qualified Scraper

-- |
-- UTCTime時間をISO8601形式にする
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%Q"

DB.share [DB.mkPersist DB.sqlSettings, DB.mkMigrate "migrateAll"] [DB.persistLowerCase|
-- |
-- サマリーテーブル
Summary
    dateTime        UTCTime -- ^ 日付時間
    quantity        Double  -- ^ 評価合計
    profit          Double  -- ^ 損益合計
    deriving Show
-- |
-- 保有株式テーブル
HoldStock
    dateTime        UTCTime -- ^ 日付時間
    code            Int     -- ^ 証券コード
    caption         String  -- ^ 名前
    count           Int     -- ^ 保有数
    purchase        Double  -- ^ 取得単価
    price           Double  -- ^ 現在値
    deriving Show
-- |
-- 余力テーブル
AssetSpare
    dateTime        UTCTime -- ^ 日付時間
    moneySpare      Int64   -- ^ 現物買付余力
    stockMoney      Int64   -- ^ 現金残高
    incDeposits     Int64   -- ^ 預り増加額
    decDeposits     Int64   -- ^ 預り減少額
    restraintFee    Int64   -- ^ ボックスレート手数料拘束金
    restraintTax    Int64   -- ^ 源泉徴収税拘束金（仮計算）
    cash            Int64   -- ^ 使用可能現金
    deriving Show
|]

DB.share [DB.mkPersist DB.sqlSettings, DB.mkMigrate "migrateTotalAssets"] [DB.persistLowerCase|
-- |
-- サマリーテーブルと
-- 保有株式テーブルを
-- 結合した総資産テーブル
TotalAssets
    dateTime        UTCTime -- ^ 日付時間
    quantity        Double  -- ^ 評価合計
    profit          Double  -- ^ 損益合計
    moneySpare      Int64   -- ^ 現物買付余力
    stockMoney      Int64   -- ^ 現金残高
    incDeposits     Int64   -- ^ 預り増加額 (株式売却時受取金)
    decDeposits     Int64   -- ^ 預り減少額 (株式買注文時拘束金及び受け渡し金)
    restraintFee    Int64   -- ^ ボックスレート手数料拘束金
    restraintTax    Int64   -- ^ 源泉徴収税拘束金（仮計算）
    cash            Int64   -- ^ 使用可能現金
    deriving Show
|]

-- |
-- 総資産（現金換算）を返す関数
-- 株式資産評価合計 + 使用可能現金
--
-- 以下の手数料関係は夕方バッチ処理で決まるので算入しない
-- 預り増加額
-- 預り減少額
-- ボックスレート手数料拘束金
-- 源泉徴収税拘束金（仮計算）
totalAssetsOfCash :: TotalAssets -> Double
totalAssetsOfCash ta =
    totalAssetsQuantity ta          -- 株式資産評価合計
    + realToFrac (
        totalAssetsCash ta          -- 使用可能現金
     )

-- |
-- サマリーテーブルと保有株式テーブルを
-- 日付時間フィールドでinner joinした
-- 総資産テーブルを逆順（最新が先頭）で取り出す関数
getTotalAstsDescList :: Maybe (String, UTCTime) -> Int -> Int -> IO [TotalAssets]
getTotalAstsDescList predicade limit offset =
    let sql = T.toStrict $ T.unwords
                [ "select"
                , " summary.id,"
                , " summary.date_time,"
                , " summary.quantity,"
                , " summary.profit,"
                , " asset_spare.money_spare,"
                , " asset_spare.stock_money,"
                , " asset_spare.inc_deposits,"
                , " asset_spare.dec_deposits,"
                , " asset_spare.restraint_fee,"
                , " asset_spare.restraint_tax,"
                , " asset_spare.cash"
                , " from summary inner join asset_spare"
                , " on (summary.date_time=asset_spare.date_time)"
                , " where summary.date_time" `has` predicade
                , " order by summary.date_time desc"
                , " limit " `T.append` T.pack (show limit)
                , " offset " `T.append` T.pack (show offset)
                , ";"
                ]
    in
    DB.runSqlite "assets.sqlite3" $
        map DB.entityVal <$> DB.rawSql sql []
    where
    has :: T.Text -> Maybe (String, UTCTime) -> T.Text
    has _ Nothing = ""
    has partial (Just (op, tm)) = T.concat
        [ partial
        , T.pack op
        , "\""
        , T.pack (iso8601 tm)
        , "\""
        ]

-- |
-- 保有株式テーブルを逆順（最新が先頭）で取り出す関数
getHoldStockDescList :: Maybe (String, UTCTime) -> IO [Scraper.HoldStock]
getHoldStockDescList predicade =
    let sql = T.toStrict $ T.unwords
                [ "select"
                , " *"
                , " from hold_stock"
                , " where hold_stock.date_time" `has` predicade
                , " order by date_time desc"
                , ";"
                ]
    in
    DB.runSqlite "assets.sqlite3" $
        DB.rawSql sql []
        >>= return . map (fromHoldStock . DB.entityVal)
    where
    has :: T.Text -> Maybe (String, UTCTime) -> T.Text
    has _ Nothing = ""
    has partial (Just x) =
        let (op, tm) = x in
        let op' = T.pack op in
        let tm' = T.pack $ iso8601 tm in
        T.concat [partial, op', "\"", tm', "\""]

-- |
-- DBへ格納する関数
insertDB d =
    DB.runSqlite "assets.sqlite3" $ do
        DB.runMigration migrateAll
        M.void (DB.insert d)

--
-- 型の変換関数
toSummary :: UTCTime -> Scraper.FraStkSell -> Summary
toSummary time (Scraper.FraStkSell qua pro _) =
    Summary time qua pro

toHoldStock :: UTCTime -> Scraper.HoldStock -> HoldStock
toHoldStock time (Scraper.HoldStock _ cod cap cou pur pri) =
    HoldStock time cod (T.unpack cap) cou pur pri

fromHoldStock :: HoldStock -> Scraper.HoldStock
fromHoldStock (HoldStock _ cod cap cou pur pri) =
    Scraper.HoldStock Nothing cod (T.pack cap) cou pur pri

toAssetSpare :: UTCTime -> Scraper.FraAstSpare -> AssetSpare
toAssetSpare time (Scraper.FraAstSpare mts som inc dec rfe rta cas) =
    AssetSpare time mts som inc dec rfe rta cas

-- |
-- サマリーテーブル, 保有株式テーブルへ格納する関数
storeStkSell :: UTCTime -> Scraper.FraStkSell -> IO ()
storeStkSell time fas = do
    insertDB $ toSummary time fas
    M.mapM_ (insertDB . toHoldStock time) $ Scraper.fsStocks fas

-- |
-- 余力テーブルへ格納する関数
storeAssetSpare :: UTCTime -> Scraper.FraAstSpare -> IO ()
storeAssetSpare time fss =
    insertDB $ toAssetSpare time fss

--
--  型クラスScraper.Contentsのインスタンスをここで定義する
instance Scraper.Contents Scraper.OrderConfirmed where
    storeToDB _ _ = undefined

instance Scraper.Contents Scraper.FraHomeAnnounce where
    -- こんなんDBに残しても意味ないんとちゃうか？
    storeToDB _ _ = undefined

instance Scraper.Contents Scraper.FraStkSell where
    storeToDB = storeStkSell

instance Scraper.Contents Scraper.FraAstSpare where
    storeToDB = storeAssetSpare

