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
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

証券会社とやりとりするモジュールです。
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module GenBroker
    ( Broker(..)
    ) where

import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Trans.Resource as MR
import qualified Data.Conduit                 as C
import qualified Data.Text.Lazy               as TL
import qualified Database.Persist.MySQL       as MySQL

import qualified BrokerBackend                as BB
import           Conf                         (InfoBroker (..),
                                               InfoMatsuiCoJp (..),
                                               InfoSBIsecCoJp (..))
import qualified MatsuiCoJp.Broker            as MatsuiCoJp
import qualified SBIsecCoJp.Broker            as SBIsecCoJp
import qualified SinkSlack

-- |
-- 型クラス
class Broker account where
    -- |
    -- runResourceTと組み合わせて証券会社のサイトにログイン/ログアウトする
    siteConn    :: (Monad m, M.MonadTrans t, MR.MonadResource (t m))
                => account
                -> String
                -> (BB.HTTPSession -> m b)
                -> t m b
    -- |
    -- Slackへお知らせを送るついでに現在資産評価をDBへ
    noticeOfBrokerageAnnouncement   :: M.MonadIO m
                                    => account
                                    -> MySQL.ConnectInfo
                                    -> String
                                    -> C.Source m TL.Text
    -- |
    -- DBから最新の資産評価を取り出してSlackへレポートを送る
    noticeOfCurrentAssets   :: M.MonadIO m
                            => account
                            -> MySQL.ConnectInfo
                            -> C.Source m SinkSlack.Report
    -- |
    -- 現在資産評価を証券会社のサイトから取得してDBへ
    fetchUpdatedPriceAndStore   :: account
                                -> MySQL.ConnectInfo
                                -> BB.HTTPSession
                                -> IO ()

--
--
instance Broker InfoMatsuiCoJp where
    siteConn = MatsuiCoJp.siteConn
    noticeOfBrokerageAnnouncement = MatsuiCoJp.noticeOfBrokerageAnnouncement
    noticeOfCurrentAssets _ = MatsuiCoJp.noticeOfCurrentAssets
    fetchUpdatedPriceAndStore _ = MatsuiCoJp.fetchUpdatedPriceAndStore
--
--
instance Broker InfoSBIsecCoJp where
    siteConn = SBIsecCoJp.siteConn
    noticeOfBrokerageAnnouncement = SBIsecCoJp.noticeOfBrokerageAnnouncement
    noticeOfCurrentAssets _ = SBIsecCoJp.noticeOfCurrentAssets
    fetchUpdatedPriceAndStore _ = SBIsecCoJp.fetchUpdatedPriceAndStore

--
--
instance Broker InfoBroker where
    siteConn = \case
        (MatsuiCoJp a) -> siteConn a
        (SBIsecCoJp a) -> siteConn a
    noticeOfBrokerageAnnouncement = \case
        (MatsuiCoJp a) -> noticeOfBrokerageAnnouncement a
        (SBIsecCoJp a) -> noticeOfBrokerageAnnouncement a
    noticeOfCurrentAssets = \case
        (MatsuiCoJp a) -> noticeOfCurrentAssets a
        (SBIsecCoJp a) -> noticeOfCurrentAssets a
    fetchUpdatedPriceAndStore = \case
        (MatsuiCoJp a) -> fetchUpdatedPriceAndStore a
        (SBIsecCoJp a) -> fetchUpdatedPriceAndStore a

