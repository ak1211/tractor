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
Module      :  MatsuiCoJp.Model
Description :  schema of table
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

松井証券サイトデータベースのモデル
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
module MatsuiCoJp.Model
    ( module MatsuiCoJp.Model
    , module ModelDef
    ) where

import           Data.Int            (Int32)
import           Data.Time           (UTCTime)
import           Database.Persist.TH

import           ModelDef

share [mkPersist sqlSettings, mkMigrate "migrateMatsuiCoJp"] [persistLowerCase|
-- |
-- 資産テーブル
MatsuicojpAsset
    at              UTCTime             -- ^ 日付時間
    evaluation      Double              -- ^ 保有株式評価合計
    profit          Double              -- ^ 保有株式損益合計
    --
    moneySpare      Int32               -- ^ 現物買付余力
    cashBalance     Int32               -- ^ 現金残高
    depositInc      Int32               -- ^ 預り増加額
    depositDec      Int32               -- ^ 預り減少額
    bindingFee      Int32               -- ^ ボックスレート手数料拘束金
    bindingTax      Int32               -- ^ 源泉徴収税拘束金（仮計算）
    cash            Int32               -- ^ 使用可能現金
    deriving Eq
-- |
-- 保有株式テーブル
MatsuicojpStock
    asset           MatsuicojpAssetId   -- ^ 紐付け
    ticker          TickerSymbol        -- ^ ティッカー
    caption         String              -- ^ 名前
    count           Int                 -- ^ 保有数
    purchase        Double              -- ^ 取得単価
    price           Double              -- ^ 現在値
    deriving Eq
|]

-- |
-- 資産テーブルの要約
matsuicojpAssetDigest :: MatsuicojpAsset -> String
matsuicojpAssetDigest v =
    show (matsuicojpAssetAt v)
    ++ ", "
    ++ "評価合計 " ++ show (matsuicojpAssetEvaluation v)
    ++ ", "
    ++ "損益合計 " ++ show (matsuicojpAssetProfit v)
    ++ ", "
    ++ "現物買付余力 " ++ show (matsuicojpAssetMoneySpare v)
    ++ ", "
    ++ "現金残高 " ++ show (matsuicojpAssetCashBalance v)
    ++ ", "
    ++ "預り増加額 " ++ show (matsuicojpAssetDepositInc v)
    ++ ", "
    ++ "預り減少額 " ++ show (matsuicojpAssetDepositDec v)
    ++ ", "
    ++ "ボックスレート手数料拘束金 " ++ show (matsuicojpAssetBindingFee v)
    ++ ", "
    ++ "源泉徴収税拘束金（仮計算） " ++ show (matsuicojpAssetBindingTax v)
    ++ ", "
    ++ "使用可能現金 " ++ show (matsuicojpAssetCash v)

-- |
-- 保有株式の要約
matsuicojpStockDigest :: MatsuicojpStock -> String
matsuicojpStockDigest v =
    (ticker $ matsuicojpStockTicker v) ++ " " ++ (matsuicojpStockCaption v)
    ++ ", "
    ++ "保有 " ++ show (matsuicojpStockCount v)
    ++ ", "
    ++ "取得 " ++ show (matsuicojpStockPurchase v)
    ++ ", "
    ++ "現在 " ++ show (matsuicojpStockPrice v)
    ++ ", "
    ++ "損益 " ++ show (matsuicojpStockGain v)
    where
    ticker (TSTYO c) = show c
    ticker TSNI225 = "日経平均株価"
    ticker TSTOPIX = "東証株価指数"
    ticker TSJPXNI400 = "JPX400"

-- |
-- 保有株式の損益
matsuicojpStockGain :: MatsuicojpStock -> Double
matsuicojpStockGain v =
    (matsuicojpStockPrice v - matsuicojpStockPurchase v)
    * realToFrac (matsuicojpStockCount v)

