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
-- 型クラスShowのインスタンス
-- 資産テーブル
instance Show MatsuicojpAsset where
    show a =
        show (matsuicojpAssetAt a)
        ++ ", "
        ++ "評価合計 " ++ show (matsuicojpAssetEvaluation a)
        ++ ", "
        ++ "損益合計 " ++ show (matsuicojpAssetProfit a)
        ++ ", "
        ++ "現物買付余力 " ++ show (matsuicojpAssetMoneySpare a)
        ++ ", "
        ++ "現金残高 " ++ show (matsuicojpAssetCashBalance a)
        ++ ", "
        ++ "預り増加額 " ++ show (matsuicojpAssetDepositInc a)
        ++ ", "
        ++ "預り減少額 " ++ show (matsuicojpAssetDepositDec a)
        ++ ", "
        ++ "ボックスレート手数料拘束金 " ++ show (matsuicojpAssetBindingFee a)
        ++ ", "
        ++ "源泉徴収税拘束金（仮計算） " ++ show (matsuicojpAssetBindingTax a)
        ++ ", "
        ++ "使用可能現金 " ++ show (matsuicojpAssetCash a)
 
-- |
-- 型クラスShowのインスタンス
-- 保有株式テーブル
instance Show MatsuicojpStock where
    show hs =
        show (matsuicojpStockTicker hs) ++ " " ++ (matsuicojpStockCaption hs)
        ++ ", "
        ++ "保有 " ++ show (matsuicojpStockCount hs)
        ++ ", "
        ++ "取得 " ++ show (matsuicojpStockPurchase hs)
        ++ ", "
        ++ "現在 " ++ show (matsuicojpStockPrice hs)
        ++ ", "
        ++ "損益 " ++ show gain
        where
        --
        --
        delta = matsuicojpStockPrice hs - matsuicojpStockPurchase hs
        --
        --
        gain :: Double
        gain =
            realToFrac (matsuicojpStockCount hs) * delta


