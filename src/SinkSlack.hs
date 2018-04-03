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
Module      :  SinkSlack
Description :  Send a message via Slack
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

Slack Web API <https://api.slack.com/web>によって
SlackとAPI接続するモジュールです。
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module SinkSlack
    ( StockDigest (..)
    , WebHook (..)
    , Attachment (..)
    , Report (..)
    , sink
    , simpleTextMsg
    , reportMsg
    ) where

import qualified Control.Monad.IO.Class     as M
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.TH              as Aeson
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Conduit               as C
import qualified Data.Text.Lazy             as TL
import qualified Data.Time                  as Tm
import qualified Data.Time.Clock.POSIX      as Tm
import qualified Network.HTTP.Conduit       as N
import qualified Network.HTTP.Types.Status  as N

import qualified Conf

-- |
--  Slack Web API - Incoming Webhooks
--  <https://api.slack.com/incoming-webhooks>
--  によるJSON構造定義
data WebHook = WebHook
    { channel     :: String
    , username    :: String
    , attachments :: [Attachment]
    , hText       :: String
    , icon_emoji  :: String
    } deriving Show

-- |
--  Slack Web API - Attaching content and links to messages
--  <https://api.slack.com/docs/message-attachments>
--  によるJSON構造定義
data Attachment = Attachment
    { color       :: String
    , pretext     :: Maybe String
    , title       :: Maybe String
    , text        :: String
    , footer      :: String
    , footer_icon :: String
    , ts          :: Integer
    } deriving Show

$(Aeson.deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \label -> if label == "hText" then "text" else label
    } ''WebHook)
$(Aeson.deriveJSON Aeson.defaultOptions ''Attachment)


data StockDigest = StockDigest
    { stockDigestAt   :: Tm.UTCTime
    , stockDigestGain :: Double
    , stockDigestMsg  :: String
    }

-- |
--  定期的に送信するレポート
data Report = Report
    { reportAt           :: Tm.UTCTime       -- ^ DB上の時間
    , reportAllAsset     :: Double           -- ^ 総資産
    , reportGrowthToday  :: Maybe Double     -- ^ 総資産増減(前日比)
    , reportAllProfit    :: Double           -- ^ 損益合計
    , reportStockDigests :: [StockDigest]    -- ^ 保有株式
    }

-- |
-- Slackへ送信する関数
send :: M.MonadIO m => Conf.InfoSlack -> BS8.ByteString -> m N.Status
send conf json =
    M.liftIO $ do
        req <- request
        manager <- N.newManager N.tlsManagerSettings
        N.responseStatus <$> N.httpLbs req manager
    where
    request =
        N.urlEncodedBody
            [("Content-type", "application/json"), ("payload", json)]
        <$> N.parseRequest (Conf.webHookURL conf)

-- |
--  Slackへ流す関数
sink :: Conf.InfoSlack -> C.ConduitT WebHook C.Void IO ()
sink conf =
    C.await >>= go
    where
    --
    --
    go Nothing = return ()
    go (Just webhook) =
        send conf (json webhook) >> C.await >>= go
    --
    --
    json = BSL8.toStrict . Aeson.encode

-- |
--  ただのテキストをSlackに送るJSONを組み立てる関数
simpleTextMsg :: M.MonadIO m => Conf.InfoSlack -> C.ConduitT TL.Text WebHook m ()
simpleTextMsg conf =
    C.await >>= go
    where
    --
    --
    go Nothing = return ()
    go (Just message) =
        C.yield (webHook message) >> C.await >>= go
    --
    --
    webHook txt = WebHook
        { channel       = Conf.channel conf
        , username      = Conf.userName conf
        , attachments   = []
        , hText         = TL.unpack txt
        , icon_emoji    = ":tractor:"
        }

-- |
--  資産情報をSlackに送るJSONを組み立てる関数
reportMsg   :: M.MonadIO m
            => Conf.InfoSlack
            -> C.ConduitT Report WebHook m ()
reportMsg conf =
    C.await >>= go
    where
    --
    --
    go Nothing = return ()
    go (Just report) =
        C.yield (webHook report) >> C.await >>= go
    --
    --
    webHook report = WebHook
        { channel       = Conf.channel conf
        , username      = Conf.userName conf
        , attachments   = map packAttach $ reportStockDigests report
        , hText         = headline report
        , icon_emoji    = ":tractor:"
        }
    --
    -- Slackメッセージの先頭行
    headline report =
        trend (reportGrowthToday report)
        ++
        " 総資産 " ++ show (reportAllAsset report)
        ++
        " 損益合計 " ++ show (reportAllProfit report)
        where
        trend Nothing = ""
        trend (Just v)
            | v < 0     = downEmoji ++ "前日比 " ++ show v
            | otherwise = upEmoji ++ "前日比 +" ++ show v
        --
        downEmoji = ":chart_with_downwards_trend:"
        upEmoji = ":chart_with_downwards_trend:"
    --
    --
    packAttach :: StockDigest -> Attachment
    packAttach stock = Attachment
        { color         = colorUpdown
        , pretext       = Nothing
        , title         = Nothing
        , text          = stockDigestMsg stock
        , footer        = Conf.userName conf
        , footer_icon   = "https://platform.slack-edge.com/img/default_application_icon.png"
        , ts            = round . Tm.utcTimeToPOSIXSeconds $ stockDigestAt stock
        }
        where
        --
        --
        colorUpdown | stockDigestGain stock < 0 = "#0034FF"
                    | otherwise                 = "#F63200"

