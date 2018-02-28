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
Module      :  KabuCom.Model
Description :  schema of table
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

Kabu.Com証券サイトデータベースのモデル
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module KabuCom.Model
    ( module KabuCom.Model
    , module ModelDef
    ) where

import           Data.Int                         (Int32)
import           Data.Monoid                      (mempty, (<>))
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TLB
import qualified Data.Text.Lazy.Builder.Int       as TLB
import qualified Data.Text.Lazy.Builder.RealFloat as TLB
import           Data.Time                        (UTCTime)
import           Database.Persist.TH

import           ModelDef

share [mkPersist sqlSettings, mkMigrate "migrateKabuCom"] [persistLowerCase|
-- |
-- 資産テーブル
KabucomAsset
    at              UTCTime             -- ^ 日付時間
    evaluation      Double              -- ^ 保有株式評価合計
    profit          Double              -- ^ 保有株式損益合計
    --
    moneySpare      Int32               -- ^ 現物買付余力
    cashBalance     Int32               -- ^ 残高(保証金現金)
    deriving Eq
-- |
-- 保有株式テーブル
KabucomStock
    asset           KabucomAssetId      -- ^ 紐付け
    at              UTCTime             -- ^ 日付時間
    ticker          TickerSymbol        -- ^ ティッカー
    caption         String              -- ^ 名前
    count           Int32               -- ^ 保有数
    purchase        Double              -- ^ 取得単価
    price           Double              -- ^ 現在値
    deriving Eq
|]

-- |
-- 資産テーブルの要約
kabucomAssetDigest :: KabucomAsset -> String
kabucomAssetDigest KabucomAsset{..} =
    TL.unpack . TLB.toLazyText $ mempty
    <> TLB.fromString (show kabucomAssetAt) <> ", "
    <> "評価合計 " <> TLB.realFloat kabucomAssetEvaluation <> ", "
    <> "損益合計 " <> TLB.realFloat kabucomAssetProfit <> ", "
    <> "現物買付余力 " <> TLB.decimal kabucomAssetMoneySpare <> ", "
    <> "現金残高 " <> TLB.decimal kabucomAssetCashBalance <> ", "

-- |
-- 保有株式の要約
kabucomStockDigest :: KabucomStock -> String
kabucomStockDigest stk@KabucomStock{..} =
    TL.unpack . TLB.toLazyText $ mempty
    <> TLB.fromString (showTickerSymbol kabucomStockTicker) <> " "
    <> TLB.fromString kabucomStockCaption <> ", "
    <> "保有 " <> TLB.decimal kabucomStockCount <> ", "
    <> "取得 " <> TLB.realFloat kabucomStockPurchase <> ", "
    <> "現在 " <> TLB.realFloat kabucomStockPrice <> ", "
    <> "損益 " <> TLB.realFloat (kabucomStockGain stk)

-- |
-- 保有株式の損益
kabucomStockGain :: KabucomStock -> Double
kabucomStockGain KabucomStock{..} =
    (kabucomStockPrice - kabucomStockPurchase)
    * realToFrac kabucomStockCount

