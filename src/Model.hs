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
Module      :  Model
Description :  A schema of Table
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

株価データベースのモデル
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model
    ( module Model
    , module ModelDef
    )
where
import           Data.ByteString.Char8                    ( ByteString )
import           Data.Int                                 ( Int64 )
import           Data.Text                                ( Text )
import           Data.Time                                ( UTCTime )
import           Database.Persist.TH

import           ModelDef

--
-- 銘柄, 時系列株価, テクニカル指標テーブル
--
share [mkPersist sqlSettings, mkMigrate "migrateQuotes"] [persistLowerCase|
-- |
-- 株式銘柄テーブル
Portfolio
    ticker      TickerSymbol    -- ^ ティッカー
    caption     Text Maybe      -- ^ 銘柄名
    updateAt    UTCTime Maybe   -- ^ 価格情報を取り込んだ日付時間
    deriving Show Eq
-- |
-- 初値, 高値, 安値, 終値, 出来高テーブル
Ohlcv
    ticker      TickerSymbol    -- ^ ティッカー
    tf          TimeFrame       -- ^ 時間枠
    at          UTCTime         -- ^ 日付時間
    open        Double Maybe    -- ^ 初値
    high        Double Maybe    -- ^ 高値
    low         Double Maybe    -- ^ 安値
    close       Double Maybe    -- ^ 終値
    volume      Int64           -- ^ 出来高
    source      Text Maybe      -- ^ 情報の入手元
    deriving Show Eq
-- |
-- テクニカル指標テーブル
TechInds
    ohlcv       OhlcvId         -- ^ 紐付け
    ind         TechnicalInds   -- ^ テクニカル指標
    val         Double          -- ^ 値
    deriving Show Eq
|]

--
-- HTTPアクセスログテーブル
--
share [mkPersist sqlSettings, mkMigrate "migrateAccessLog"] [persistLowerCase|
-- |
-- HTTP通信記録用データーベース
-- urlのページにHTTP通信をした時の返答
AccessLog
    receivedAt      UTCTime sqltype=DATETIME(6) -- ^ 受信時間(マイクロ秒精度)
    --
    url             String      -- ^ ページのURL
    scheme          String      -- ^ スキーム
    userInfo        String      -- ^ オーソリティ
    host            String      -- ^ オーソリティ
    port            String      -- ^ オーソリティ
    path            String      -- ^ パス
    query           String      -- ^ クエリ
    fragment        String      -- ^ フラグメント
    --
    reqCookie       String      -- ^ 要求クッキー
    reqHeader       String      -- ^ 要求ヘッダ
    --
    respStatus      String      -- ^ 返答HTTP status code
    respVersion     String      -- ^ 返答HTTP version
    respHeader      String      -- ^ 返答ヘッダ
    respCookie      String      -- ^ 返答クッキー
    respBody        ByteString  -- ^ 返答ボディ(HTML)
    deriving Show
|]
