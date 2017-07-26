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
module Lib where

import qualified Data.Maybe as M

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Network.HTTP.Types.Header as N
import qualified Data.Time as Tm

import qualified Conf

-- | 日本時間(JST) = UTC+9
jst :: Tm.TimeZone
jst = Tm.hoursToTimeZone 9

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

