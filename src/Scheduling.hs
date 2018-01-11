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
Module      :  Scheduling
Description :  This file is main module of Application "Tractor"
Copyright   :  (c) 2017-2018 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

スケジューラです。
-}
{-# LANGUAGE LambdaCase #-}
module Scheduling
    ( JobFunc
    , Job
    , packZonedTimeJob
    , packLocalTimeJob
    , execute
    , tradingTimeOfTSEInJST
    , announceWeekdayTimeInJST
    , announceHolidayTimeInJST
    , batchProcessTimeInJST
    , tomorrowMidnightJST
    ) where

import qualified Control.Arrow          as A
import qualified Control.Concurrent     as CC
import qualified Data.List              as L
import qualified Data.Time              as Tm
import qualified Data.Time.Clock.System as Tm

import qualified Lib

type JobFunc = IO ()
type Job = (Tm.UTCTime, JobFunc)

--
packZonedTimeJob :: (Tm.ZonedTime, JobFunc) -> Job
packZonedTimeJob = A.first Tm.zonedTimeToUTC

--
packLocalTimeJob :: Tm.TimeZone -> (Tm.LocalTime, JobFunc) -> Job
packLocalTimeJob tz =
    packZonedTimeJob
    . A.first (flip Tm.ZonedTime tz)

-- |
-- スケジュール実行関数
execute :: [Job] -> JobFunc
execute =
    loop . map (A.first toSecondsOnlySystemTime) . sort'
    where
    sort' =
        L.sortBy (\l r -> compare (fst l) (fst r))
    --
    toSecondsOnlySystemTime :: Tm.UTCTime -> Tm.SystemTime
    toSecondsOnlySystemTime = secondsOnly . Tm.utcToSystemTime
    --
    secondsOnly t = t { Tm.systemNanoseconds = 0 }
    --
    loop :: [(Tm.SystemTime, JobFunc)] -> IO ()
    loop [] = return ()
    loop continue@((tm,fn):next) =
        fmap secondsOnly Tm.getSystemTime
        >>= \case
        now | now <  tm -> wait (distance tm now) >> loop continue  -- まだ実行時間ではない
            | now == tm -> fn >> loop next                          -- 実行
            | otherwise -> loop next                                -- すでに過ぎたのでキャンセル
    --
    distance a b = abs (Tm.systemSeconds a - Tm.systemSeconds b)
    --
    delayMilliSec ms= CC.threadDelay (ms * 1000)
    delaySec s      = CC.threadDelay (s * 1000 * 1000)
    --
    wait s
        | s == 0    = return ()
        | s == 1    = delayMilliSec 20
        | s == 2    = delayMilliSec 100
        | s <  60   = delaySec (fromIntegral s - 1)
        | otherwise = delaySec 60

-- |
-- (開始時間, 終了時間)から開始より終了まで１秒づつ増やしたリストを作る
listOfSeconds :: (Tm.TimeOfDay, Tm.TimeOfDay) -> [Tm.TimeOfDay]
listOfSeconds =
    map (Tm.timeToTimeOfDay . Tm.picosecondsToDiffTime)
    . (\(beg,end) -> [beg, beg+step .. end])
    . (toPicosec A.*** toPicosec)
    where
    --    milli  micro   nano   pico
    step = 1000 * 1000 * 1000 * 1000    -- one seconds
    --
    toPicosec = Tm.diffTimeToPicoseconds . Tm.timeOfDayToTime

-- |
-- 現物株式の立会時間(東京証券取引所,日本時間)
tradingTimeOfTSEInJST :: Tm.Day -> ([Tm.ZonedTime], [Tm.ZonedTime])
tradingTimeOfTSEInJST jstDay =
    (lists morning, lists afternoon)
    where
    tol = Tm.LocalTime jstDay
    lists = map (flip Tm.ZonedTime Lib.tzJST . tol) . listOfSeconds
    --
    morning     = (Tm.TimeOfDay  9 00 00, Tm.TimeOfDay 11 30 00)
    afternoon   = (Tm.TimeOfDay 12 30 00, Tm.TimeOfDay 15 00 00)

-- |
-- 平日の報告時間
announceWeekdayTimeInJST :: Tm.Day -> [Tm.ZonedTime]
announceWeekdayTimeInJST jstDay =
    map (flip Tm.ZonedTime Lib.tzJST . Tm.LocalTime jstDay)
    [ Tm.TimeOfDay  6 30 00
    , Tm.TimeOfDay 11 35 00
    , Tm.TimeOfDay 15 05 00
    ]

-- |
-- 休日の報告時間
announceHolidayTimeInJST :: Tm.Day -> [Tm.ZonedTime]
announceHolidayTimeInJST jstDay =
    map (flip Tm.ZonedTime Lib.tzJST . Tm.LocalTime jstDay)
    [ Tm.TimeOfDay  6 30 00 ]

-- |
-- バッチ作業時間
batchProcessTimeInJST :: Tm.Day -> [Tm.ZonedTime]
batchProcessTimeInJST jstDay =
    map (flip Tm.ZonedTime Lib.tzJST . Tm.LocalTime jstDay)
    [ Tm.TimeOfDay 18 20 00 ]

-- |
-- 明日の日本時間午前0時
tomorrowMidnightJST :: Tm.Day -> Tm.ZonedTime
tomorrowMidnightJST =
    flip Tm.ZonedTime Lib.tzJST         -- JST ZonedTime
    . flip Tm.LocalTime Tm.midnight     -- JST Tomorrow 00:00:00
    . succ                              -- JST Today + 1 (a.k.a Tomorrow)

