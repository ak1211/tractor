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
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( greetingsMessage
    , tzJST
    , httpRequestHeader
    , parseJSTDayTimeToUTCTime
    , NthOfTotal
    , packNthOfTotal
    , every
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe                 (catMaybes)
import qualified Data.Maybe                 as M
import qualified Data.Text.Lazy             as TL
import qualified Data.Time                  as Tm
import qualified Network.HTTP.Types.Header  as N

import qualified Conf

-- |
-- 起動時の挨拶文
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

-- |
-- 日本時間(JST) = UTC+9
tzJST :: Tm.TimeZone
tzJST =
    let z = Tm.hoursToTimeZone 9 in
    z {Tm.timeZoneName = "JST"}

-- |
-- HTTPリクエストヘッダ
httpRequestHeader :: Conf.Info -> [N.Header]
httpRequestHeader conf =
    [ (N.hAccept, "text/html, text/plain, text/css")
    , (N.hAcceptCharset, "UTF-8")
    , (N.hAcceptLanguage, "ja, en;q=0.5")
    , (N.hUserAgent, BL8.toStrict $ BL8.pack $ Conf.userAgent conf)
    ]

-- |
-- 日本時間の日付時間をパースする
parseJSTDayTimeToUTCTime :: String -> Maybe String -> Tm.UTCTime
parseJSTDayTimeToUTCTime date mTime =
    let t = M.fromMaybe "00:00" mTime in
    let dtm = date ++ "T" ++ t ++ ":00+0900" in
    let format = Tm.iso8601DateFormat (Just "%H:%M:%S%z") in
    Tm.parseTimeOrError True Tm.defaultTimeLocale format dtm

-- | (nth, total)
type NthOfTotal = (Int,Int)

-- |
-- リスト中のn番目情報を追加する関数
--
-- >>> packNthOfTotal "abcdefg"
-- [('a',(1,7)),('b',(2,7)),('c',(3,7)),('d',(4,7)),('e',(5,7)),('f',(6,7)),('g',(7,7))]
packNthOfTotal :: [a] -> [(a, NthOfTotal)]
packNthOfTotal vs =
    zip vs nth
    where
    nth = [(n, length vs) | n<-[1..]]

-- |
-- リストからn個毎に取り出す
--
-- >>> every 3 [1..50]
-- [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49]
every :: Int -> [a] -> [a]
every n =
    catMaybes . zipWith
        (\idx val ->
            if idx `mod` n == 0
            then Just val
            else Nothing)
        [0..]

