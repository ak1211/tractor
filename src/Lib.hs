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
Description :  library
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Lib
    ( greetingsMessage
    , tzAsiaTokyo
    , httpRequestHeader
    , parseJSTDayTimeToUTCTime
    , toISO8601DateTime
    , NthOfTotal
    , packNthOfTotal
    , every
    ) where
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Maybe                 as Mb
import qualified Data.Text.Lazy             as TL
import qualified Data.Time                  as Tm
import qualified Network.HTTP.Types.Header  as N

import           VerRev                     (versionString)

-- |
-- 起動時の挨拶文
greetingsMessage :: TL.Text
greetingsMessage = TL.unlines
    [ TL.pack $ "tractor " ++ versionString ++ " が起動しました。"
    , "以降一定時間で通知します。"
    , "tractor is an Assets observation application."
    , "*tractor © 2016 Akihiro Yamamoto.*"
    , "このプログラムは *全くの無保証* で提供されます。"
    , "これはフリーソフトウェアであり、ある条件の下で再頒布することが奨励されています。"
    , "詳しくは https://github.com/ak1211/tractor をご覧ください。"
    ]

-- |
-- 日本時間(JST) = UTC+9
tzAsiaTokyo :: Tm.TimeZone
tzAsiaTokyo =
    let z = Tm.hoursToTimeZone 9 in
    z {Tm.timeZoneName = "JST"}

-- |
-- HTTPリクエストヘッダ
httpRequestHeader :: String -> [N.Header]
httpRequestHeader ua =
    [ (N.hAccept, "text/html, text/plain, text/css")
    , (N.hAcceptCharset, "UTF-8")
    , (N.hAcceptLanguage, "ja, en;q=0.5")
    , (N.hUserAgent, BL8.toStrict $ BL8.pack ua)
    ]

-- |
-- 日本時間の日付時間をパースする
parseJSTDayTimeToUTCTime :: String -> Maybe String -> Tm.UTCTime
parseJSTDayTimeToUTCTime date mTime =
    let
        t = Mb.fromMaybe "00:00" mTime
        dtm = date ++ "T" ++ t ++ ":00+0900"
        format = Tm.iso8601DateFormat (Just "%H:%M:%S%z")
    in
    Tm.parseTimeOrError True Tm.defaultTimeLocale format dtm

-- |
-- 日本時間の日付時間へ
toISO8601DateTime :: Tm.ZonedTime -> String
toISO8601DateTime zt =
    let format = Tm.iso8601DateFormat (Just "%H:%M:%S%z")
    in
    Tm.formatTime Tm.defaultTimeLocale format zt

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
-- >>> every 1 [0..25]
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- >>> every 2 [0..50]
-- [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50]
-- >>> every 3 [0..50]
-- [0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48]
-- >>> every 4 [0..50]
-- [0,4,8,12,16,20,24,28,32,36,40,44,48]
-- >>> every 5 [0..50]
-- [0,5,10,15,20,25,30,35,40,45,50]
-- >>> every 1 [1..25]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- >>> every 2 [1..50]
-- [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49]
-- >>> every 3 [1..50]
-- [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49]
-- >>> every 4 [1..50]
-- [1,5,9,13,17,21,25,29,33,37,41,45,49]
-- >>> every 5 [1..50]
-- [1,6,11,16,21,26,31,36,41,46]
every :: Int -> [a] -> [a]
every n =
    map snd . (filter fst . zip bs)
    where
    bs = cycle $ take n (True : repeat False)


