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
Module      :  ModelDef
Description :
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
module ModelDef
    where
import qualified Control.Arrow          as A
import           Control.Exception.Safe
import           Data.Char              (toUpper)
import           Data.Word              (Word32)
import           Database.Persist.TH    (derivePersistField)
import           Text.Read              (readMaybe)

--
--
data TickerSymbol
    = TSTYO Word32  -- ^ 東証:個別株
    | TSNI225       -- ^ 日経平均株価
    | TSTOPIX       -- ^ TOPIX
    deriving (Show, Read, Eq)
derivePersistField "TickerSymbol"

--
--
showTickerSymbol :: TickerSymbol -> String
showTickerSymbol (TSTYO c) = "東" ++ show c
showTickerSymbol TSNI225   = "日経平均株価"
showTickerSymbol TSTOPIX   = "東証株価指数"

-- |
--
-- >>> toTickerSymbol "998407.O"
-- TSNI225
-- >>> toTickerSymbol "998405.T"
-- TSTOPIX
-- >>> toTickerSymbol "8306.T"
-- TSTYO 8306
-- >>> toTickerSymbol "8306.t"
-- TSTYO 8306
-- >>> toTickerSymbol "8306t" :: Maybe TickerSymbol
-- Nothing
--
toTickerSymbol :: MonadThrow m => String -> m TickerSymbol
toTickerSymbol codeStr =
    case go $ map toUpper codeStr of
        Just a  -> pure a
        Nothing-> throwString $ "toTickerSymbol: no parse of \"" ++ codeStr ++ "\""
    where
    --
    --
    go "998407.O" = Just TSNI225
    go "998405.T" = Just TSTOPIX
    go cs = do
        (code, market) <- parse cs
        case market of
            "T" -> Just $ TSTYO code
            _   -> Nothing
    --
    --
    parse :: String -> Maybe (Word32, String)
    parse cs =
        let (c, m) = A.second (drop 1) $ span (/= '.') cs
        in
        (,) <$> readMaybe c <*> pure m

-- |
--
-- >>> fromTickerSymbol TSNI225
-- "998407.O"
-- >>> fromTickerSymbol TSTOPIX
-- "998405.T"
-- >>> fromTickerSymbol (TSTYO 8306)
-- "8306.T"
--
fromTickerSymbol :: TickerSymbol -> String
fromTickerSymbol TSNI225    = "998407.O"
fromTickerSymbol TSTOPIX    = "998405.T"
fromTickerSymbol (TSTYO c)  = show c ++ ".T"

--
--
data TimeFrame
    = TF1h  -- ^ 1時間足
    | TF1d  -- ^ 日足
    deriving (Show, Read, Eq)
derivePersistField "TimeFrame"

--
--
showTimeFrame :: TimeFrame -> String
showTimeFrame TF1h = "１時間足"
showTimeFrame TF1d = "日足"

--
--
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


