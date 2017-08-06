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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TechnicalIndicators where

import qualified Data.List as List
import Database.Persist.TH

data TechnicalInds
    = TISMA             Int         -- ^| 単純移動平均(SMA)
    | TIEMA             Int         -- ^| 指数平滑移動平均(EMA)
    | TIRSI             Int         -- ^| 相対力指数(RSI)
    | TIMACD            Int Int     -- ^| MACD
    | TIMACDSIG         Int Int Int -- ^| MACDシグナル
    | TIBBLOW3          Int         -- ^| ボリンジャーバンド(-3σ)
    | TIBBLOW2          Int         -- ^| ボリンジャーバンド(-2σ)
    | TIBBLOW1          Int         -- ^| ボリンジャーバンド(-1σ)
    | TIBBMIDDLE        Int         -- ^| ボリンジャーバンド
    | TIBBUP1           Int         -- ^| ボリンジャーバンド(+1σ)
    | TIBBUP2           Int         -- ^| ボリンジャーバンド(+2σ)
    | TIBBUP3           Int         -- ^| ボリンジャーバンド(+3σ)
    | TIPSYCHOLO        Int         -- ^| サイコロジカルライン
    | TIDIPOS           Int         -- ^| +DI
    | TIDINEG           Int         -- ^| -DI
    | TIADX             Int         -- ^| ADX
    deriving (Show, Read, Eq)

derivePersistField "TechnicalInds"

-- | 入力を一つずらしながら期間のグループを作る
rolling :: Int -> [Double] -> [[Double]]
rolling n | n < 1 = error "期間は１以上でね"
rolling n =
    List.unfoldr
    (\case
    vs | length vs < n  -> Nothing
       | otherwise      -> Just (take n vs, drop 1 vs)
    )

-- | グループ毎に平均を計算する
mean :: Int -> [[Double]] -> [Double]
mean period | period < 1 = error "期間は１以上でね"
mean period =
    map formula
    where
    formula xs | length xs /= period = error $ "期間" ++ show period
                                            ++ "と値の数" ++ show (length xs)
                                            ++ "が一致していないよ"
    formula xs = sum xs / realToFrac period

-- | 単純移動平均(SMA)
--   最新を先頭にした時系列から計算する
sma :: Int -> [Double] -> [Double]
sma period = mean period . rolling period

-- | 指数平滑移動平均(EMA)
--   最新を先頭にした時系列から引数の前日emaを元にemaを計算する
ema :: Int -> Double -> [Double] -> [Double]
ema period _  _ | period < 1 = error "期間は１以上でね"
ema period seedEma serialsDesc =
    scanr formula seedEma serialsDesc
    where
    --
    alpha = 2.0 / realToFrac (period + 1)
    --
    formula todayPrice yesterdayEMA =
        yesterdayEMA + alpha * (todayPrice - yesterdayEMA)

-- | 指数平滑移動平均(EMA)
--   最新を先頭にした時系列から計算する
ema1 :: Int -> [Double] -> [Double]
ema1 period serialsDesc =
    let (header,body) = splitAt period $ reverse serialsDesc in
    let (seedEma:_) = mean period [header] in
    ema period seedEma $ reverse body

-- | 前との差
differences     :: [Double] -> [Double]
differencesDesc :: [Double] -> [Double]
--
differences     xs = zipWith (-) (drop 1 xs) xs
differencesDesc xs = zipWith (-) xs $ drop 1 xs

-- | 相対力指数(RSI)
--   最新を先頭にした時系列から計算する
rsi :: Int -> [Double] -> [Double]
rsi period _ | period < 1   = error "期間は１以上でね"
rsi period serialsDesc =
    [ gain / (gain + loss) * 100.0
    | ps<-rolling period $ differencesDesc serialsDesc
    , let gain = sum [abs v | v<-ps, v >= 0]
    , let loss = sum [abs v | v<-ps, v <  0]
    ]

-- | サイコロジカルライン
--   最新を先頭にした時系列から計算する
psycologicalLine :: Int -> [Double] -> [Double]
psycologicalLine period _ | period < 1 = error "期間は１以上でね"
psycologicalLine period serialsDesc =
    [ ascend / realToFrac period * 100.0
    | window<-rolling period $ differencesDesc serialsDesc
    , let ascend = sum [1.0 | n<-window, n >= 0]
    ]

--
macdFormula :: Double -> Double -> Double
macdFormula fastEma slowEma = fastEma - slowEma

-- | MACD
--   最新を先頭にした時系列から計算する
macd :: Int -> Int -> [Double] -> [Double]
macd fastPeriod slowPeriod serialsDesc =
    zipWith macdFormula (ema1 fastPeriod serialsDesc) (ema1 slowPeriod serialsDesc)

--
macdSignalFormula :: Int -> [Double] -> [Double]
macdSignalFormula = sma

-- | MACD signal
--   最新を先頭にした時系列から計算する
macdSignal :: Int -> Int -> Int -> [Double] -> [Double]
macdSignal fastPeriod slowPeriod signalPeriod =
    macdSignalFormula signalPeriod . macd fastPeriod slowPeriod

type BBand321LowerMiddle123Upper = (Double,Double,Double, Double, Double,Double,Double)
-- | ボリンジャーバンド
--   最新を先頭にした時系列から計算する
bollingerBands :: Int -> [Double] -> [BBand321LowerMiddle123Upper]
bollingerBands period _ | period < 1 = error "期間は１以上でね"
bollingerBands period serialsDesc =
    let low3    = zipWith (\m s -> m - s * 3.0) middle sigma in
    let low2    = zipWith (\m s -> m - s * 2.0) middle sigma in
    let low1    = zipWith (\m s -> m - s * 1.0) middle sigma in
    --
    let up1     = zipWith (\m s -> m + s * 1.0) middle sigma in
    let up2     = zipWith (\m s -> m + s * 2.0) middle sigma in
    let up3     = zipWith (\m s -> m + s * 3.0) middle sigma in
    List.zip7 low3 low2 low1 middle up1 up2 up3
    where
    --
    window :: [[Double]]
    window = rolling period serialsDesc
    --
    middle :: [Double]
    middle = mean period window
    --
    sigma :: [Double]
    sigma = [sigmaFormula w | w<-window]
    --
    sigmaFormula :: [Double] -> Double
    sigmaFormula price =
        let sumOfSquared    = sum [x ** 2.0 | x<-price] in
        let squaredOfSums   = sum price ** 2.0 in
        let pd = realToFrac period in
        let n = pd * sumOfSquared - squaredOfSums in
        let d = pd * (pd - 1.0) in
        sqrt (n / d)

-- | +-DM
--   最新を先頭にした時系列から計算する
pnDM :: [Double] -> [Double] -> [(Double,Double)]
pnDM serialsDescHi serialsDescLo =
    zipWith formula diffHi diffLo
    where
    diffHi = differencesDesc serialsDescHi
    diffLo = differences serialsDescLo
    --
    formula dHi dLo
        | dHi == dLo            = (0.0, 0.0)
        | dHi > 0 && dLo < 0    = (dHi, 0.0)
        | dHi < 0 && dLo > 0    = (0.0, dLo)
        | dHi > dLo             = (dHi, 0.0)
        | dHi < dLo             = (0.0, dLo)
        | otherwise = error "missing condition"

-- | TrueRange
--   最新を先頭にした時系列から計算する
trueRange :: [PriceHiLoClo] -> [Double]
trueRange serialsDesc =
    zipWith formula serialsDesc $ drop 1 serialsDesc
    where
    formula (currentHi,currentLo,_) (_,_,previousClose) =
        maximum [ currentHi - currentLo
                , currentHi - previousClose
                , previousClose - currentLo
                ]

type PriceHiLoClo = (Double,Double,Double)
type DiPosNegADX = (Double,Double,Double)
-- | DMI
--   最新を先頭にした時系列から計算する
dmi :: Int -> [PriceHiLoClo] -> [DiPosNegADX]
dmi period serialsDesc =
    let pos = rolling period positiveDM in
    let neg = rolling period negativeDM in
    let diPos = zipWith (\x y -> sum x / sum y * 100.0) pos tr in
    let diNeg = zipWith (\x y -> sum x / sum y * 100.0) neg tr in
    let dx = zipWith (\p n -> abs (p - n) / (p + n) * 100.0) diPos diNeg in
    let adx = sma period dx in
    zip3 diPos diNeg adx
    where
    priceHi = [a | (a,_,_)<-serialsDesc]
    priceLo = [b | (_,b,_)<-serialsDesc]
    positiveDM = map fst $ pnDM priceHi priceLo
    negativeDM = map snd $ pnDM priceHi priceLo
    tr = rolling period $ trueRange serialsDesc


