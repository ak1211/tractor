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
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

スケジューラです。

>>> :set -XOverloadedStrings
-}
{-# LANGUAGE LambdaCase #-}
module Scheduling
    ( AsiaTokyoDay(..)
    , JobFunc
    , JobTime
    , Job
    , Jobs
    , ZonedTimeJob
    , ZonedTimeJobs
    , packZonedTimeJob
    , unpackZonedTimeJob
    , unpackLocalTimeJob
    , executeZT
    , execute
    , tomorrowMidnight
    , addTimeOfSeconds
    , addTimeOfSecondsZT
    , tradingTimeOfSecondsTSE
    , announceWeekdayTime
    , announceHolidayTime
    , batchProcessTime
    ) where
import qualified Control.Arrow          as A
import qualified Control.Concurrent     as CC
import qualified Data.List              as L
import qualified Data.Time              as Tm
import qualified Data.Time.Clock.System as Tm

import           Lib                    (tzAsiaTokyo)

newtype AsiaTokyoDay = AsiaTokyoDay
    { getAsiaTokyoDay :: Tm.Day }
    deriving (Eq, Ord, Show)

type JobFunc = IO ()
type JobTime = Tm.UTCTime
type Job = (JobTime, JobFunc)
type Jobs = [Job]
type ZonedTimeJob = (Tm.ZonedTime, JobFunc)
type ZonedTimeJobs = [ZonedTimeJob]

--
packZonedTimeJob :: Tm.TimeZone -> Job -> ZonedTimeJob
packZonedTimeJob tz =
    A.first (Tm.utcToZonedTime tz)

--
unpackZonedTimeJob :: ZonedTimeJob -> Job
unpackZonedTimeJob =
    A.first Tm.zonedTimeToUTC

--
unpackLocalTimeJob :: Tm.TimeZone -> (Tm.LocalTime, JobFunc) -> Job
unpackLocalTimeJob tz =
    unpackZonedTimeJob . A.first (flip Tm.ZonedTime tz)

-- |
-- スケジュール実行関数
executeZT :: ZonedTimeJobs -> JobFunc
executeZT = execute . map unpackZonedTimeJob

-- |
-- スケジュール実行関数
execute :: Jobs -> JobFunc
execute =
    loop . map (A.first toSecondsOnlySystemTime) . sort'
    where
    --
    --
    sort' =
        L.sortBy (\l r -> compare (fst l) (fst r))
    --
    --
    toSecondsOnlySystemTime :: Tm.UTCTime -> Tm.SystemTime
    toSecondsOnlySystemTime = secondsOnly . Tm.utcToSystemTime
    --
    --
    secondsOnly t = t { Tm.systemNanoseconds = 0 }
    --
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
    --
    distance a b = abs (Tm.systemSeconds a - Tm.systemSeconds b)
    --
    --
    delayMilliSec ms= CC.threadDelay (ms * 1000)
    delaySec s      = CC.threadDelay (s * 1000 * 1000)
    --
    --
    wait s
        | s == 0    = return ()
        | s == 1    = delayMilliSec 20
        | s == 2    = delayMilliSec 100
        | s <  60   = delaySec (fromIntegral s - 1)
        | otherwise = delaySec 60

-- |
-- 明日の日本時間午前0時
tomorrowMidnight :: AsiaTokyoDay -> Tm.ZonedTime
tomorrowMidnight =
    flip Tm.ZonedTime tzAsiaTokyo   -- JST ZonedTime
    . flip Tm.LocalTime Tm.midnight     -- JST Tomorrow 00:00:00
    . succ                              -- JST Today + 1 (a.k.a Tomorrow)
    . getAsiaTokyoDay

-- |
-- UTC時間の加減算
addTimeOfSeconds :: Integer -> Job -> Job
addTimeOfSeconds seconds =
    A.first $ Tm.addUTCTime (fromInteger seconds)

-- |
-- UTC時間の加減算
addTimeOfSecondsZT :: Integer -> ZonedTimeJob -> ZonedTimeJob
addTimeOfSecondsZT seconds job@(zt,_) =
    go (Tm.zonedTimeZone zt) job
    where
    go z =
        packZonedTimeJob z . addTimeOfSeconds seconds . unpackZonedTimeJob

-- |
-- (開始時間, 終了時間)から開始より終了まで１秒づつ増やしたリストを作る
-- >>> secondsSequence (Tm.TimeOfDay  1 00 00, Tm.TimeOfDay 1 00 05)
-- [01:00:00,01:00:01,01:00:02,01:00:03,01:00:04,01:00:05]
secondsSequence :: (Tm.TimeOfDay, Tm.TimeOfDay) -> [Tm.TimeOfDay]
secondsSequence =
    map (Tm.timeToTimeOfDay . Tm.picosecondsToDiffTime)
    . (\(beg,end) -> [beg, beg+step .. end])
    . (toPicosec A.*** toPicosec)
    where
    --    milli  micro   nano   pico
    step = 1000 * 1000 * 1000 * 1000    -- one seconds
    --
    toPicosec = Tm.diffTimeToPicoseconds . Tm.timeOfDayToTime

--
--
toZonedTime :: AsiaTokyoDay -> Tm.TimeOfDay -> Tm.ZonedTime
toZonedTime day =
    flip Tm.ZonedTime tzAsiaTokyo . Tm.LocalTime (getAsiaTokyoDay day)

-- |
-- 現物株式の立会時間(東京証券取引所,日本時間)
tradingTimeOfSecondsTSE :: AsiaTokyoDay -> ([Tm.ZonedTime], [Tm.ZonedTime])
tradingTimeOfSecondsTSE day =
    (mapping morning, mapping afternoon)
    where
    mapping = map (toZonedTime day) . secondsSequence
    --
    morning     = (Tm.TimeOfDay  9 00 00, Tm.TimeOfDay 11 30 00)
    afternoon   = (Tm.TimeOfDay 12 30 00, Tm.TimeOfDay 15 00 00)

-- |
-- 平日の報告時間
announceWeekdayTime :: AsiaTokyoDay -> [Tm.ZonedTime]
announceWeekdayTime day =
    map (toZonedTime day)
    [ Tm.TimeOfDay  6 30 00
    , Tm.TimeOfDay 11 40 00
    , Tm.TimeOfDay 15 20 00
    ]

-- |
-- 休日の報告時間
announceHolidayTime :: AsiaTokyoDay -> [Tm.ZonedTime]
announceHolidayTime day =
    map (toZonedTime day)
    [ Tm.TimeOfDay  6 30 00 ]

-- |
-- バッチ作業時間
batchProcessTime :: AsiaTokyoDay -> [Tm.ZonedTime]
batchProcessTime day =
    map (toZonedTime day)
    []

