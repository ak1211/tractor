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
Module      :  TechnicalIndicators
Description :  
Copyright   :  (c) 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE TemplateHaskell #-}

module TechnicalIndicators where

import qualified Safe
import qualified Control.Monad as M
import qualified Data.List as List
import qualified Data.Maybe as Mb
import Database.Persist.TH

data TechnicalInds
    = TISMA             Int         -- ^| 単純移動平均(SMA)
    | TIEMA             Int         -- ^| 指数平滑移動平均(EMA)
    | TIRSI             Int         -- ^| 相対力指数(RSI)
    | TIMACD            Int Int     -- ^| MACD
    | TIMACDSIG         Int Int Int -- ^| MACDシグナル
    | TIPSYCHOLO        Int         -- ^| サイコロジカルライン
    deriving (Show, Read, Eq)

derivePersistField "TechnicalInds"

-- | 単純移動平均(SMA)
--   リストの先頭から計算する版
sma :: Int -> [Double] -> [Maybe Double]
sma period prices
    | period < 1 = error "期間は１以上でね"
    | length prices < period = replicate (length prices) Nothing
    | otherwise =
        let (seeds, ps) = splitAt period prices in
        let na = replicate (period-1) Nothing in
        let val = snd $ sma' period (reverse seeds) ps in
        na ++ map Just val

type SmaAccumlator = [Double]
-- | 単純移動平均(SMA)
--   初期のアキュムレータを入力と別々に渡して計算する版
sma'    :: Int
        -> SmaAccumlator                -- ^| 最新の値が先頭の並びで渡す事
        -> [Double]                     -- ^| 入力は時系列通りで渡す事
        -> (SmaAccumlator, [Double])    -- ^| 計算を打ち切った時のアキュムレータの値と結果のタプル
sma' period seeds prices
    | period < 1               = error "期間は１以上でね"
    | length seeds /= period   = error "アキュムレータの数と周期が一致していないよ"
    | otherwise =
        let (acc, xs) = List.mapAccumL go seeds prices in
        let lst = snd $ go acc 0.0 in  -- 最後にアキュムレータに残った値を押し出す
        ( acc
        , xs ++ [lst]
        )
    where
    go :: SmaAccumlator -> Double -> (SmaAccumlator, Double)
    go acc price =  ( price : init acc
                    , sum acc / realToFrac period
                    )

-- | 指数平滑移動平均(EMA)
--   リストの先頭から計算する版
ema :: Int -> [Double] -> [Maybe Double]
ema period prices
    | period < 1 = error "期間は１以上でね"
    | length prices < period = replicate (length prices) Nothing
    | otherwise =
        let (seeds, ps) = splitAt period prices in
        let seed = sum seeds / realToFrac period in
        let na = replicate (period-1) Nothing in
        let val = ema' period seed ps in
        na ++ map Just val

-- | 指数平滑移動平均(EMA)
--   種を入力と別々に渡して計算する版
ema' :: Int -> Double -> [Double] -> [Double]
ema' period seed prices
    | period < 1 = error "期間は１以上でね"
    | otherwise =
        scanl formula seed prices
    where
    --
    alpha = 2.0 / realToFrac (period + 1)
    --
    formula :: Double -> Double -> Double
    formula yesterdayEMA todayPrice =
        yesterdayEMA + alpha * (todayPrice - yesterdayEMA)

-- | MACD
--   リストの先頭から計算する版
macd :: Int -> Int -> [Double] -> [Maybe Double]
macd fastPeriod slowPeriod prices =
    zipWith (M.liftM2 formula) fast slow
    where
    fast = ema fastPeriod prices
    slow = ema slowPeriod prices
    formula f s = f - s

-- | MACD signal
--   リストの先頭から計算する版
macdSignal :: Int -> Int -> Int -> [Double] -> [Maybe Double]
macdSignal fastPeriod slowPeriod signalPeriod prices =
    let (na, mbvs) = span Mb.isNothing thisMACD in
    let vs = map (Safe.fromJustNote "SMAの定義により、中間にNothingは無いはず") mbvs in
    na ++ sma signalPeriod vs
    where
    thisMACD = macd fastPeriod slowPeriod prices

-- | 相対力指数(RSI)
--   リストの先頭から計算する版
rsi :: Int -> [Double] -> [Maybe Double]
rsi period prices
    | period < 1 = error "期間は１以上でね"
    | length prices <= period = replicate (length prices) Nothing
    | otherwise =
        let (seeds, ps) = splitAt (period+1) prices in
        let diffs = zipWith (-) (drop 1 seeds) seeds in
        let seed = RsiAccumlator
                    { rsiAscend     = sum [abs v | v<-diffs, v >= 0] / realToFrac period
                    , rsiDescend    = sum [abs v | v<-diffs, v < 0] / realToFrac period
                    , rsiLastPrice  = Safe.lastNote "たぶんこの例外は発生しないかな" seeds
                    }
        in
        let val = snd $ rsi' period seed ps in
        let na = replicate period Nothing in
        na ++ map Just val

data RsiAccumlator = RsiAccumlator
    { rsiAscend     :: Double
    , rsiDescend    :: Double
    , rsiLastPrice  :: Double
    } deriving (Show, Eq, Ord)
-- | 相対力指数(RSI)
--   初期のアキュムレータを入力と別々に渡して計算する版
rsi'    :: Int
        -> RsiAccumlator
        -> [Double]
        -> (RsiAccumlator, [Double])    -- ^| 計算を打ち切った時のアキュムレータの値と結果のタプル
rsi' period seed prices
    | period < 1 = error "期間は１以上でね"
    | otherwise =
        let (acc, xs) = List.mapAccumL go seed prices in
        let lst = snd $ go acc 0.0 in  -- 最後にアキュムレータに残った値を押し出す
        ( acc
        , xs ++ [lst]
        )
    where
    --
    go :: RsiAccumlator -> Double -> (RsiAccumlator, Double)
    go acc price =
        let a = rsiAscend acc * realToFrac (period - 1) in
        let d = rsiDescend acc * realToFrac (period - 1) in
        let t = case price - rsiLastPrice acc of
                x | x >= 0      -> RsiAccumlator
                                    { rsiAscend     = (a + abs x) / realToFrac period
                                    , rsiDescend    = d / realToFrac period
                                    , rsiLastPrice  = price
                                    }
                  | otherwise   -> RsiAccumlator
                                    { rsiAscend     = a / realToFrac period
                                    , rsiDescend    = (d + abs x) / realToFrac period
                                    , rsiLastPrice  = price
                                    }
        in
        ( t
        , rsiAscend acc / (rsiAscend acc + rsiDescend acc) * 100.0
        )

-- | サイコロジカルライン
--   リストの先頭から計算する版
psycologicalLine :: Int -> [Double] -> [Maybe Double]
psycologicalLine period prices
    | period < 1 = error "期間は１以上でね"
    | length prices <= period = replicate (length prices) Nothing
    | otherwise =
        let (seeds, ps) = splitAt (period+1) prices in
        let diffs = zipWith (-) (drop 1 seeds) seeds in
        let seed = PsycoloAccumlator
                    { psycoloDiffs      = reverse diffs
                    , psycoloLastPrice  = Safe.lastNote "たぶんこの例外は発生しないかな" seeds
                    }
        in
        let val = snd $ psycologicalLine' period seed ps in
        let na = replicate period Nothing in
        na ++ map Just val

data PsycoloAccumlator = PsycoloAccumlator
    { psycoloDiffs      :: [Double]
    , psycoloLastPrice  :: Double
    } deriving (Show, Eq, Ord)
-- | サイコロジカルライン
--   初期のアキュムレータを入力と別々に渡して計算する版
psycologicalLine'   :: Int
                    -> PsycoloAccumlator                -- ^| 最新の値が先頭の並びで渡す事
                    -> [Double]                         -- ^| 入力は時系列通りで渡す事
                    -> (PsycoloAccumlator, [Double])    -- ^| 計算を打ち切った時のアキュムレータの値と結果のタプル
psycologicalLine' period seed prices
    | period < 1 = error "期間は１以上でね"
    | otherwise =
        let (acc, xs) = List.mapAccumL go seed prices in
        let lst = snd $ go acc 0.0 in  -- 最後にアキュムレータに残った値を押し出す
        ( acc
        , xs ++ [lst]
        )
    where
    --
    go :: PsycoloAccumlator -> Double -> (PsycoloAccumlator, Double)
    go (PsycoloAccumlator diffs lastPrice) price =
        let t = PsycoloAccumlator
                    { psycoloDiffs      = (price - lastPrice) : init diffs
                    , psycoloLastPrice  = price
                    }
        in
        ( t
        , sum [1 | x<-diffs, x>=0] / realToFrac period * 100.0
        )

