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
Module      :  SlackBot
Description :  Send a message via Slack
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

Slack Web API <https://api.slack.com/web>によって
SlackとAPI接続するモジュールです。
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SinkSlack
    ( WebHook (..)
    , Attachment (..)
    , Report (..)
    , sink
    , simpleTextMsg
    , reportMsg
    ) where

import qualified Control.Monad              as M
import qualified Control.Monad.IO.Class     as M
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.TH              as Aeson
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Conduit               as C
import qualified Data.Maybe                 as Maybe
import qualified Data.Text.Lazy             as TL
import qualified Data.Time                  as Tm
import qualified Data.Time.Clock.POSIX      as Tm
import qualified Network.HTTP.Conduit       as N
import qualified Network.HTTP.Types.Status  as N
import qualified Text.Printf                as Printf

import qualified Conf
import           MatsuiCoJp.Model

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

$(Aeson.deriveJSON Aeson.defaultOptions {
        Aeson.fieldLabelModifier = \label -> if label == "hText" then "text" else label
    } ''WebHook)
$(Aeson.deriveJSON Aeson.defaultOptions ''Attachment)

-- |
--  定期的に送信するレポート
data Report = Report
    { rTime           :: Tm.UTCTime           -- ^ DB上の時間
    , rAllAsset       :: Double               -- ^ 総資産
    , rAssetDiffByDay :: Maybe Double         -- ^ 総資産増減(前日比)
    , rAllProfit      :: Double               -- ^ 損益合計
    , rHoldStocks     :: [MatsuicojpStock]    -- ^ 保有株式
    } deriving Show

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
sink :: Conf.InfoSlack -> C.Sink WebHook IO ()
sink conf =
    C.await >>= maybe (return ()) go
    where
    go webhook = do
        M.void $ send conf (BSL8.toStrict $ Aeson.encode webhook)
        C.await >>= maybe (return ()) go


-- |
--  ただのテキストをSlackに送るJSONを組み立てる関数
simpleTextMsg :: M.MonadIO m => Conf.InfoSlack -> C.Conduit TL.Text m WebHook
simpleTextMsg conf =
    C.await >>= maybe (return ()) go
    where
    go message = do
        C.yield WebHook {
            channel       = Conf.channel conf,
            username      = Conf.userName conf,
            attachments   = [],
            hText         = TL.unpack message,
            icon_emoji    = ":tractor:"
        }
        C.await >>= maybe (return ()) go

-- |
--  資産情報をSlackに送るJSONを組み立てる関数
reportMsg :: M.MonadIO m => Conf.InfoSlack -> C.Conduit Report m WebHook
reportMsg conf =
    C.await >>= maybe (return ()) go
    where
    --
    --
    go rpt = do
        C.yield WebHook
            { channel       = Conf.channel conf
            , username      = Conf.userName conf
            , attachments   = map (mkAttach $ rTime rpt) $ rHoldStocks rpt
            , hText         = unwords . Maybe.catMaybes $
                                [ updown <$> rAssetDiffByDay rpt
                                , Printf.printf "前日比 %+f " <$> rAssetDiffByDay rpt
                                , Just (Printf.printf "総資産 %f " $ rAllAsset rpt)
                                , Just (Printf.printf "損益合計 %+f" $ rAllProfit rpt)
                                ]
            , icon_emoji    = ":tractor:"
            }
        C.await >>= maybe (return ()) go
    --
    --
    updown :: Double -> String
    updown x
        | x < 0         = ":chart_with_downwards_trend:"
        |     0 < x     = ":chart_with_upwards_trend:"
        | otherwise     = ":chart_with_upwards_trend:"
    --
    --
    gain :: MatsuicojpStock -> Double
    gain s =
        (matsuicojpStockPrice s - matsuicojpStockPurchase s)
        * realToFrac (matsuicojpStockCount s)
    --
    --
    mkAttach :: Tm.UTCTime -> MatsuicojpStock -> Attachment
    mkAttach time stock = Attachment
        { color         = if gain stock >= 0 then "#F63200" else "#0034FF"
        , pretext       = Nothing
        , title         = Nothing
        , text          = show stock
        , footer        = Conf.userName conf
        , footer_icon   = "https://platform.slack-edge.com/img/default_application_icon.png"
        , ts            = round $ Tm.utcTimeToPOSIXSeconds time
        }

