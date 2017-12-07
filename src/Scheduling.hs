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
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
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
    , execute
    , toUTCTimeFromJST
    , morningSessionTSE
    , afternoonSessionTSE
    , reportSecuritiesAnnounceTimeFromJST
    , batchProcessTimeFromJST
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

-- | スケジュール実行関数
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
        now | now <  tm -> wait (distance tm now) >> loop continue
            | now == tm -> fn >> loop next
            | otherwise -> loop next
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

--
toTimeOfDaySequence :: (Tm.TimeOfDay, Tm.TimeOfDay) -> [Tm.TimeOfDay]
toTimeOfDaySequence =
    map (Tm.timeToTimeOfDay . Tm.picosecondsToDiffTime)
    . (\(beg,end) -> [beg, beg+step .. end])
    . (toPicosec A.*** toPicosec)
    where
    --    milli  micro   nano   pico
    step = 1000 * 1000 * 1000 * 1000    -- one seconds
    --
    toPicosec = Tm.diffTimeToPicoseconds . Tm.timeOfDayToTime

-- | 明日の午前0時(日本時間)
tomorrowMidnightJST :: Tm.Day -> Tm.UTCTime
tomorrowMidnightJST =
    Tm.zonedTimeToUTC                   -- UTC
    . flip Tm.ZonedTime Lib.tzJST       -- JST local to zoned
    . flip Tm.LocalTime Tm.midnight     -- JST Tomorrow 00:00:00
    . succ                              -- JST Today + 1 (a.k.a Tomorrow)

--
toUTCTimeFromJST :: Tm.Day -> Tm.TimeOfDay -> Tm.UTCTime
toUTCTimeFromJST jstDay jstTime =
    Lib.fromLocalTimeJST $ Tm.LocalTime jstDay jstTime

-- | 現物株式の立会時間(東京証券取引所,日本時間,前場)
morningSessionTSE :: [Tm.TimeOfDay]
morningSessionTSE = toTimeOfDaySequence
    (Tm.TimeOfDay  9 00 00, Tm.TimeOfDay 11 30 00)

-- | 現物株式の立会時間(東京証券取引所,日本時間,後場)
afternoonSessionTSE :: [Tm.TimeOfDay]
afternoonSessionTSE = toTimeOfDaySequence
    (Tm.TimeOfDay 12 30 00, Tm.TimeOfDay 15 00 00)

-- | "お知らせ"の報告時間
reportSecuritiesAnnounceTimeFromJST :: Tm.Day -> [Tm.UTCTime]
reportSecuritiesAnnounceTimeFromJST jstDay = map (toUTCTimeFromJST jstDay)
    [ Tm.TimeOfDay  6 30 00
    , Tm.TimeOfDay 11 35 00
    , Tm.TimeOfDay 15 05 00
    ]

-- | バッチ作業時間
batchProcessTimeFromJST :: Tm.Day -> [Tm.UTCTime]
batchProcessTimeFromJST jstDay = map (toUTCTimeFromJST jstDay)
    [Tm.TimeOfDay 17 20 00]

