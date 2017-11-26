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
Module      :  Lib
Description :  
Copyright   :  (c) 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
module Lib
    ( greetingsMessage
    , doSleepThread
    , tzJST
    , httpRequestHeader
    , parseJSTDayTimeToUTCTime
    , NthOfTotal
    , packNthOfTotal
    ) where

import qualified Data.Maybe as M

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Network.HTTP.Types.Header as N
import qualified Data.Time as Tm

import qualified Data.Text.Lazy as TL

import qualified Control.Concurrent as CC

import qualified Conf

-- | 起動時の挨拶文
greetingsMessage :: TL.Text
greetingsMessage = TL.unlines
    [ "tractorが起動しました"
    , "以降一定時間で通知します。"
    , "tractor is an Assets observation application."
    , "*tractor © 2016, 2017 Akihiro Yamamoto.*"
    , "このプログラムは *全くの無保証* で提供されます。"
    , "これはフリーソフトウェアであり、ある条件の下で再頒布することが奨励されています。"
    , "詳しくは https://github.com/ak1211/tractor をご覧ください。"
    ]

-- | 時間調整でスレッドを停止する関数
--   停止時間の単位は分
doSleepThread :: Int -> IO ()
doSleepThread minutes = CC.threadDelay (minutes * 60 * 1000 * 1000)

-- | 日本時間(JST) = UTC+9
tzJST :: Tm.TimeZone
tzJST = Tm.hoursToTimeZone 9

-- | HTTPリクエストヘッダ
httpRequestHeader :: Conf.Info -> [N.Header]
httpRequestHeader conf =
    [ (N.hAccept, "text/html, text/plain, text/css")
    , (N.hAcceptCharset, "UTF-8")
    , (N.hAcceptLanguage, "ja, en;q=0.5")
    , (N.hUserAgent, BL8.toStrict $ BL8.pack $ Conf.userAgent conf)
    ]

-- | 日本時間の日付時間をパースする
parseJSTDayTimeToUTCTime :: String -> Maybe String -> Tm.UTCTime
parseJSTDayTimeToUTCTime date mTime =
    let t = M.fromMaybe "00:00" mTime in
    let dtm = date ++ "T" ++ t ++ ":00+0900" in
    let format = Tm.iso8601DateFormat (Just "%H:%M:%S%z") in
    Tm.parseTimeOrError True Tm.defaultTimeLocale format dtm

-- | (nth, total)
type NthOfTotal = (Int,Int)

-- | リスト中のn番目情報を追加する関数
packNthOfTotal :: [a] -> [(a, NthOfTotal)]
packNthOfTotal vs =
    zip vs nth
    where
    nth = [(n, length vs) | n<-[1..]]

