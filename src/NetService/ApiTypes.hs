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
Module      :  NetService.ApiTypes
Description :  Type definition of network services
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

ネットワークモジュールで使う型の定義
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
module NetService.ApiTypes
    ( ServerTChan
    , Portfolio
    , toPortfolio
    , Ohlcv
    , toOhlcv
    )
    where
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson             as Aeson
import qualified Data.Csv               as Csv
import           Data.Int               (Int64)
import qualified Data.Text              as T
import qualified Data.Time              as Time
import           GHC.Generics           (Generic)
import qualified Servant.Docs
import qualified Servant.Elm

import           Lib                    (toISO8601DateTime, tzAsiaTokyo)
import qualified Model

--
--
type ServerTChan = STM.TChan [Ohlcv]

-- |
-- ポートフォリオ
data Portfolio = Portfolio
    { code     :: String
    , caption  :: Maybe String
    , updateAt :: Maybe String
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Portfolio
instance Aeson.ToJSON Portfolio
instance Servant.Elm.ElmType Portfolio
instance Servant.Docs.ToSample Portfolio where
    toSamples _ =
        Servant.Docs.singleSample $
        Portfolio "TYO8306" (Just "三菱ＵＦＪフィナンシャル・グループ") Nothing

--
--
toPortfolio :: Model.Portfolio -> Portfolio
toPortfolio o = Portfolio
    { code     = Model.fromTickerSymbol $ Model.portfolioTicker o
    , caption  = T.unpack <$> Model.portfolioCaption o
    , updateAt = toISO8601DateTime
                . Time.utcToZonedTime tzAsiaTokyo
                <$> Model.portfolioUpdateAt o
    }

-- |
-- 4本値 + 出来高
data Ohlcv = Ohlcv
    { at     :: String
    , code   :: String
    , open   :: Maybe Double
    , high   :: Maybe Double
    , low    :: Maybe Double
    , close  :: Maybe Double
    , volume :: Int64
    , source :: Maybe String
    } deriving (Eq, Show, Generic)

-- |
-- 4本値 + 出来高
instance Aeson.FromJSON Ohlcv
instance Aeson.ToJSON Ohlcv
instance Servant.Elm.ElmType Ohlcv
instance Servant.Docs.ToSample Ohlcv where
    toSamples _ =
        Servant.Docs.singleSample v
        where
        v = Ohlcv
            { at     = "2018-03-07T15:00:00+0900"
            , code   = "TYO8306"
            , open   = Just 723
            , high   = Just 726
            , low    = Just 715
            , close  = Just 715
            , volume = 78487400
            , source = Just "This is example."
            }

-- |
-- 4本値 + 出来高
instance Csv.FromRecord Ohlcv
instance Csv.ToNamedRecord Ohlcv where
    toNamedRecord Ohlcv{..} =
        Csv.namedRecord
        [ "at"     Csv..= at
        , "code"   Csv..= code
        , "open"   Csv..= open
        , "high"   Csv..= high
        , "low"    Csv..= low
        , "close"  Csv..= close
        , "volume" Csv..= volume
        , "source" Csv..= source
        ]
instance Csv.DefaultOrdered Ohlcv where
    headerOrder _ =
        Csv.header
        [ "at"
        , "code"
        , "open"
        , "high"
        , "low"
        , "close"
        , "volume"
        , "source"
        ]

--
--
toOhlcv :: Model.Ohlcv -> Ohlcv
toOhlcv o = Ohlcv
    { at     = toISO8601DateTime
                . Time.utcToZonedTime tzAsiaTokyo
                $ Model.ohlcvAt o
    , code   = Model.fromTickerSymbol $ Model.ohlcvTicker o
    , open   = Model.ohlcvOpen o
    , high   = Model.ohlcvHigh o
    , low    = Model.ohlcvLow o
    , close  = Model.ohlcvClose o
    , volume = Model.ohlcvVolume o
    , source = T.unpack <$> Model.ohlcvSource o
    }

