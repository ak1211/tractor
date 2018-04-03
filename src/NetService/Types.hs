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
Module      :  NetService.Types
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
module NetService.Types
    where
import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.Aeson                   as Aeson
import qualified Data.Csv                     as Csv
import           Data.Int                     (Int64)
import qualified Data.Text                    as T
import qualified Data.Time                    as Tm
import qualified Data.Vector                  as Vect
import           GHC.Generics                 (Generic)

import           Lib                          (toISO8601DateTime, tzAsiaTokyo)
import           Model

--
--
type ServerTChan = TChan [ReplyOhlcv]

--
--
data ReplyPortfolio = ReplyPortfolio
    { code     :: String
    , caption  :: Maybe String
    , updateAt :: Maybe String
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON ReplyPortfolio
instance Aeson.ToJSON ReplyPortfolio

--
--
toReplyPortfolio :: Portfolio -> ReplyPortfolio
toReplyPortfolio o = ReplyPortfolio
    { code     = fromTickerSymbol $ portfolioTicker o
    , caption  = T.unpack <$> portfolioCaption o
    , updateAt = toISO8601DateTime
                . Tm.utcToZonedTime tzAsiaTokyo
                <$> portfolioUpdateAt o
    }

--
--
data ReplyOhlcv = ReplyOhlcv
    { at     :: String
    , code   :: String
    , open   :: Maybe Double
    , high   :: Maybe Double
    , low    :: Maybe Double
    , close  :: Maybe Double
    , volume :: Int64
    , source :: Maybe String
    } deriving (Eq, Show, Generic)
instance Aeson.FromJSON ReplyOhlcv
instance Aeson.ToJSON ReplyOhlcv
instance Csv.FromRecord ReplyOhlcv
instance Csv.ToNamedRecord ReplyOhlcv where
    toNamedRecord ReplyOhlcv{..} =
        Csv.namedRecord
        [ "at"     Csv..= at
        , "code"   Csv..= code
        , "open"   Csv..= open
        , "high"   Csv..= high
        , "low"    Csv..= low
        , "close"  Csv..= close
        , "close"  Csv..= close
        , "volume" Csv..= volume
        , "source" Csv..= source
        ]
replyOhlcvHeader :: Csv.Header
replyOhlcvHeader =
    Vect.fromList
    [ "at"
    , "code"
    , "open"
    , "high"
    , "low"
    , "close"
    , "close"
    , "volume"
    , "source"
    ]

--
--
toReplyOhlcv :: Ohlcv -> ReplyOhlcv
toReplyOhlcv o = ReplyOhlcv
    { at     = toISO8601DateTime
                . Tm.utcToZonedTime tzAsiaTokyo
                $ ohlcvAt o
    , code   = fromTickerSymbol $ ohlcvTicker o
    , open   = ohlcvOpen o
    , high   = ohlcvHigh o
    , low    = ohlcvLow o
    , close  = ohlcvClose o
    , volume = ohlcvVolume o
    , source = T.unpack <$> ohlcvSource o
    }

