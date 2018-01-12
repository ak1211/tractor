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
Module      :  GenBroker
Description :  generic broker
Copyright   :  (c) 2017-2018 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

証券会社とやりとりするモジュールです。
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module GenBroker
    ( Broker (..)
    ) where

import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Conduit                 as C
import qualified Data.Text.Lazy               as TL
import qualified Database.Persist.MySQL       as MySQL

import qualified BrokerBackend                as BB
import qualified Conf
import qualified MatsuiCoJp.Broker
import qualified SinkSlack

-- |
-- 型クラス
class Broker a where
    -- |
    -- runResourceTと組み合わせて証券会社のサイトにログイン/ログアウトする
    siteConn    :: (Monad m, M.MonadTrans t, MR.MonadResource (t m))
                => a
                -> String
                -> (BB.HTTPSession -> m b)
                -> t m b
    -- |
    -- Slackへお知らせを送るついでに現在資産評価をDBへ
    noticeOfBrokerageAnnouncement   :: M.MonadIO m
                                    => MySQL.ConnectInfo
                                    -> a
                                    -> String
                                    -> C.Source m TL.Text
    -- |
    -- DBから最新の資産評価を取り出してSlackへレポートを送る
    noticeOfCurrentAssets   :: M.MonadIO m
                            => MySQL.ConnectInfo
                            -> a
                            -> C.Source m SinkSlack.Report
    -- |
    -- 現在資産評価を証券会社のサイトから取得してDBへ
    fetchUpdatedPriceAndStore   :: MySQL.ConnectInfo
                                -> a
                                -> BB.HTTPSession
                                -> IO ()

--
--
instance Broker Conf.InfoMatsuiCoJp where
    --
    siteConn =
        MatsuiCoJp.Broker.siteConn
    --
    noticeOfBrokerageAnnouncement =
        MatsuiCoJp.Broker.noticeOfBrokerageAnnouncement
    --
    noticeOfCurrentAssets x _ =
        MatsuiCoJp.Broker.noticeOfCurrentAssets x
    --
    fetchUpdatedPriceAndStore x _ =
        MatsuiCoJp.Broker.fetchUpdatedPriceAndStore x

