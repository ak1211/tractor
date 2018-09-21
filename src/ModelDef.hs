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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module ModelDef where
import           Control.Exception.Safe
import           Data.Char                                ( toLower
                                                          , toUpper
                                                          )
import           Data.Fixed                               ( Centi )
import           Data.Word                                ( Word32 )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH                      ( derivePersistField )
import           GHC.Generics                             ( Generic )
import qualified Data.Text                     as T
import           Text.Read                                ( readMaybe )

--
--
newtype Currency = Currency
    { getCurrency :: Centi
    } deriving (Show, Eq, Num)

instance PersistField Currency where
    toPersistValue =
        PersistInt64 . fromIntegral . fromEnum . getCurrency
    fromPersistValue (PersistInt64 c) =
        Right . Currency . toEnum . fromIntegral $ c
    fromPersistValue x =
        Left . T.pack $ "Expected Int64 counting cents, got: " ++ show x

instance PersistFieldSql Currency where
    sqlType _ = SqlInt64

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
-- >>> toTickerSymbol "NI225"
-- TSNI225
-- >>> toTickerSymbol "TOPIX"
-- TSTOPIX
-- >>> toTickerSymbol "TYO8306"
-- TSTYO 8306
-- >>> toTickerSymbol "tyo8306"
-- TSTYO 8306
-- >>> toTickerSymbol "8306" :: Maybe TickerSymbol
-- Nothing
--
toTickerSymbol :: MonadThrow m => String -> m TickerSymbol
toTickerSymbol codeStr = case go $ map toUpper codeStr of
    Just a -> pure a
    Nothing ->
        throwString $ "toTickerSymbol: no parse of \"" ++ codeStr ++ "\""
  where
    --
    --
    go "NI225" = Just TSNI225
    go "TOPIX" = Just TSTOPIX
    go cs      = do
        (market, code) <- parse cs
        case market of
            "TYO" -> Just $ TSTYO code
            _     -> Nothing
    --
    --
    parse :: String -> Maybe (String, Word32)
    parse cs = let (m, c) = splitAt 3 cs in (,) <$> pure m <*> readMaybe c

-- |
--
-- >>> fromTickerSymbol TSNI225
-- "NI225"
-- >>> fromTickerSymbol TSTOPIX
-- "TOPIX"
-- >>> fromTickerSymbol (TSTYO 8306)
-- "TYO8306"
--
fromTickerSymbol :: TickerSymbol -> String
fromTickerSymbol TSNI225   = "NI225"
fromTickerSymbol TSTOPIX   = "TOPIX"
fromTickerSymbol (TSTYO c) = "TYO" ++ show c

-- |
--
data TimeFrame
    = TF1h  -- ^ 1時間足
    | TF1d  -- ^ 日足
    deriving (Show, Read, Eq, Generic)
derivePersistField "TimeFrame"

-- |
--
showTimeFrame :: TimeFrame -> String
showTimeFrame TF1h = "１時間足"
showTimeFrame TF1d = "日足"

-- |
--
validTimeFrames :: [String]
validTimeFrames = map fromTimeFrame [TF1h, TF1d]

-- |
--
toTimeFrame :: MonadThrow m => String -> m TimeFrame
toTimeFrame str = case go $ map toLower str of
    Just a  -> pure a
    Nothing -> throwString $ "toTimeFrame: no parse of \"" ++ str ++ "\""
  where
    go "1h" = Just TF1h
    go "1d" = Just TF1d
    go _    = Nothing

-- |
--
fromTimeFrame :: TimeFrame -> String
fromTimeFrame TF1h = "1h"
fromTimeFrame TF1d = "1d"

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


