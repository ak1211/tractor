{- |
Module      :  SlackBot.hs
Description :  Send a message via Slack
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  portable

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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module SlackBot
    ( WebHook (..)
    , Attachment (..)
    , Report (..)
    , sinkSlack
    , simpleTextMsg
    , reportMsg
    ) where

import Prelude hiding (catch)
import Control.Exception
import qualified Conf
import qualified DataBase
import qualified Scraper

import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Reader as M

import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit (($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding

import qualified Text.Printf as Printf

import qualified Network.Connection as N
import qualified Network.URI as N
import qualified Network.HTTP.Conduit
import qualified Network.HTTP.Conduit as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Method as N
import qualified Network.HTTP.Types.Status as N

import qualified Data.Maybe
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import qualified Data.Time.Clock.POSIX as POSIX

import Data.Time (UTCTime)
import qualified Data.Time as Tm
import qualified Data.Time.LocalTime as LT

{-
 - Slack Web API
 - https://api.slack.com/web
 -}

{-
 - Slack Web API - Incoming Webhooks
 - https://api.slack.com/incoming-webhooks
 -}
data WebHook = WebHook {
    channel         :: String,
    username        :: String,
    attachments     :: [Attachment],
    hText           :: String,
    icon_emoji      :: String
} deriving Show

{-
 - Slack Web API - Attaching content and links to messages
 - https://api.slack.com/docs/message-attachments
 -}
data Attachment = Attachment {
    color           :: String,
    pretext         :: Maybe String,
    title           :: Maybe String,
    text            :: String,
    footer          :: String,
    footer_icon     :: String,
    ts              :: Integer
} deriving Show

$(Aeson.deriveJSON Aeson.defaultOptions {
        Aeson.fieldLabelModifier = \label -> if label == "hText" then "text" else label
    } ''WebHook)
$(Aeson.deriveJSON Aeson.defaultOptions ''Attachment)

{-
 - 定期的に送信するレポート
 -}
data Report = Report {
    rTime           :: UTCTime,             -- DB上の時間
    rTotalAsset     :: Double,              -- 総資産
    rAssetDiffByDay :: Maybe Double,        -- 総資産増減(前日比)
    rTotalProfit    :: Double,              -- 損益合計
    rHoldStocks     :: [Scraper.HoldStock]  -- 保有株式
} deriving Show

{-
 -  slackへ送信する関数
 -}
send :: M.MonadIO m => Conf.InfoSlack -> BS8.ByteString -> m N.Status
send conf payload =
    let rBody = apiReqBody payload in
    M.liftIO $ do
        manager <- N.newManager N.tlsManagerSettings
        req <- N.parseRequest $ Conf.webHookURL conf
        let req' = N.urlEncodedBody rBody req
        resp <- N.httpLbs req' manager
        return $ N.responseStatus resp
    where
    -- Slack APIに送るリクエストボディを組み立てる関数
    apiReqBody :: BS8.ByteString -> [(BS8.ByteString, BS8.ByteString)]
    apiReqBody payload =
        [ ("Content-type", "application/json")
        , ("payload", payload)
        ]

{-
 - Slackへ流す関数
 -}
sinkSlack :: Conf.InfoSlack -> C.Sink WebHook IO ()
sinkSlack conf =
    C.await >>= Maybe.maybe (return ()) func
    where
    --
    func webhook = do
        resp <- send conf (BSL8.toStrict $ Aeson.encode webhook)
        -- respに返ってきた結果はログに落とすためだけど
        -- 今は無視する
        C.await >>= Maybe.maybe (return ()) func


{-
 - ただのテキストを
 - SlackAPIに送るJSONを組み立てる関数
 -}
simpleTextMsg :: (Monad m, M.MonadIO m) => Conf.InfoSlack -> C.Conduit T.Text m WebHook
simpleTextMsg conf =
    C.await >>= Maybe.maybe (return ()) func
    where
    --
    func message = do
        C.yield WebHook {
            channel       = Conf.channel conf,
            username      = Conf.userName conf,
            attachments   = [],
            hText         = T.unpack message,
            icon_emoji    = ":tractor:"
        }
        C.await >>= Maybe.maybe (return ()) func

{-
 - 資産情報を
 - Slackに送るJSONを組み立てる関数
 -}
reportMsg :: (Monad m, M.MonadIO m) => Conf.InfoSlack -> C.Conduit Report m WebHook
reportMsg conf =
    C.await >>= Maybe.maybe (return ()) func
    where
    --
    func (Report time total diff profit holdStocks) = do
        let msg = unwords $ Maybe.catMaybes (
                    [ updown <$> diff
                    , Printf.printf "前日比 %+f " <$> diff
                    , Printf.printf "総資産 %f " <$> Just total
                    , Printf.printf "損益合計 %+f" <$> Just profit
                    ] :: [Maybe String])
        C.yield WebHook {
            channel       = Conf.channel conf,
            username      = Conf.userName conf,
            attachments   = map (mkAtt time) holdStocks,
            hText         = msg,
            icon_emoji    = ":tractor:"
        }
        C.await >>= Maybe.maybe (return ()) func
        where
        --
        updown :: Double -> String
        updown diff
            | diff < 0  = ":chart_with_downwards_trend:"
            | otherwise = ":chart_with_upwards_trend:"

   --
    mkAtt :: UTCTime -> Scraper.HoldStock -> Attachment
    mkAtt time stock =
        Attachment  {
            color         = if Scraper.hsGain stock >= 0 then "#F63200" else "#0034FF",
            pretext       = Nothing,
            title         = Nothing,
            text          = show stock,
            footer        = Conf.userName conf,
            footer_icon   = "https://platform.slack-edge.com/img/default_application_icon.png",
            ts            = round $ POSIX.utcTimeToPOSIXSeconds time
        }
