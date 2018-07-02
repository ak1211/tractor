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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.ApiTypes
    ( ServerTChan
    , ApiPortfolio
    , toApiPortfolio
    , ApiOhlcv
    , toApiOhlcv
    , fromApiOhlcv
    , OAuthAccessResponse(..)
    , OAuthReply(..)
    , VerRev(..)
    )
    where
import qualified Control.Concurrent.STM as STM
import           Control.Monad          (guard)
import           Data.Aeson             ((.:), (.:?))
import qualified Data.Aeson             as Aeson
import           Data.Csv               ((.=))
import qualified Data.Csv               as Csv
import           Data.Int               (Int64)
import qualified Data.Text              as T
import qualified Data.Time              as Time
import           GHC.Generics           (Generic)
import qualified Servant.Docs
import qualified Servant.Elm
import qualified Web.FormUrlEncoded

import           Lib                    (toISO8601DateTime, tzAsiaTokyo)
import qualified Model

--
--
data VerRev = VerRev
    { version        :: String
    , buildArch      :: String
    , buildOS        :: String
    , gitBranch      :: String
    , gitHash        :: String
    , gitCommitDate  :: String
    , gitCommitCount :: String
    , gitStatus      :: String
    } deriving (Generic, Show)
instance Aeson.FromJSON VerRev
instance Aeson.ToJSON VerRev

instance Servant.Elm.ElmType VerRev
instance Servant.Docs.ToSample VerRev where
    toSamples _ =
        let v = VerRev
                    { version       = "0.4.9"
                    , buildArch     = "x86-64"
                    , buildOS       = "linux"
                    , gitBranch     = "master"
                    , gitHash       = "9c411f808b67e0bf71219aa771e66178699727ed"
                    , gitCommitDate = "Thu Jun 28 19:12:47 2018 +0900"
                    , gitCommitCount= "133"
                    , gitStatus     = "Dirty"
                    }
        in
        Servant.Docs.singleSample v

--
--
type ServerTChan = STM.TChan [ApiOhlcv]

-- |
-- slack api oauth.accessからのJSON返答
data OAuthAccessResponse = OAuthAccessResponse
    { respOk          :: Bool
    , respError       :: Maybe T.Text
    , respAccessToken :: Maybe T.Text
    , respScope       :: Maybe T.Text
    , respUserId      :: Maybe T.Text
    , respUserName    :: Maybe T.Text
    , respTeamId      :: Maybe T.Text
    } deriving Show

instance Aeson.FromJSON OAuthAccessResponse where
    parseJSON = Aeson.withObject "oauth.access response" $ \o -> do
        respOk          <- o .: "ok"
        respError       <- o .:? "error"
        respAccessToken <- o .:? "access_token"
        respScope       <- o .:? "scope"
        --
        -- userの中のid,nameを取り出す
        oUser           <- o .:? "user"
        respUserId      <- maybe (pure Nothing) (.:? "id") oUser
        respUserName    <- maybe (pure Nothing) (.:? "name") oUser
        --
        -- teamの中のidを取り出す
        oTeam           <- o .:? "team"
        respTeamId      <- maybe (pure Nothing) (.:? "id") oTeam
        return OAuthAccessResponse{..}

-- |
-- OAuth返答
data OAuthReply = OAuthReply
    { accessToken :: T.Text
    , scope       :: T.Text
    , userName    :: T.Text
    } deriving (Eq, Show, Generic)

instance Aeson.FromJSON OAuthReply
instance Aeson.ToJSON OAuthReply
instance Servant.Elm.ElmType OAuthReply
instance Servant.Docs.ToSample OAuthReply where
    toSamples _ =
        Servant.Docs.singleSample $ OAuthReply
            "xoxp-????????????-????????????-????????????-????????????????????????????????"
            "identify.basic"
            "John Doe"

-- |
-- ポートフォリオ
data ApiPortfolio = ApiPortfolio
    { code     :: String
    , caption  :: Maybe T.Text
    , updateAt :: Maybe String
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON ApiPortfolio
instance Aeson.ToJSON ApiPortfolio
instance Servant.Elm.ElmType ApiPortfolio
instance Servant.Docs.ToSample ApiPortfolio where
    toSamples _ =
        Servant.Docs.singleSample v
        where
        v = ApiPortfolio
            { code = "TYO8306"
            , caption = Just "三菱ＵＦＪフィナンシャル・グループ"
            , updateAt =  Nothing
            }

--
--
toApiPortfolio :: Model.Portfolio -> ApiPortfolio
toApiPortfolio o = ApiPortfolio
    { code     = Model.fromTickerSymbol $ Model.portfolioTicker o
    , caption  = Model.portfolioCaption o
    , updateAt = toISO8601DateTime
                . Time.utcToZonedTime tzAsiaTokyo
                <$> Model.portfolioUpdateAt o
    }

-- |
-- 4本値 + 出来高
data ApiOhlcv = ApiOhlcv
    { at     :: String
    , open   :: Maybe Double
    , high   :: Maybe Double
    , low    :: Maybe Double
    , close  :: Maybe Double
    , volume :: Int64
    , source :: Maybe String
    } deriving (Show, Generic)

-- |
-- 4本値 + 出来高
instance Aeson.FromJSON ApiOhlcv
instance Aeson.ToJSON ApiOhlcv
instance Web.FormUrlEncoded.FromForm ApiOhlcv
instance Web.FormUrlEncoded.ToForm ApiOhlcv
instance Servant.Elm.ElmType ApiOhlcv
instance Servant.Docs.ToSample ApiOhlcv where
    toSamples _ =
        Servant.Docs.singleSample v
        where
        v = ApiOhlcv
            { at     = "2018-03-07T15:00:00+0900"
            , open   = Just 723
            , high   = Just 726
            , low    = Just 715
            , close  = Just 715
            , volume = 78487400
            , source = Just "This is example."
            }

-- |
-- 4本値 + 出来高
instance Csv.FromRecord ApiOhlcv
instance Csv.ToNamedRecord ApiOhlcv where
    toNamedRecord ApiOhlcv{..} =
        Csv.namedRecord
        [ "at"     .= at
        , "open"   .= open
        , "high"   .= high
        , "low"    .= low
        , "close"  .= close
        , "volume" .= volume
        , "source" .= source
        ]
instance Csv.DefaultOrdered ApiOhlcv where
    headerOrder _ =
        Csv.header
        [ "at"
        , "open"
        , "high"
        , "low"
        , "close"
        , "volume"
        , "source"
        ]

--
--
toApiOhlcv :: Model.Ohlcv -> ApiOhlcv
toApiOhlcv Model.Ohlcv{..} = ApiOhlcv
    { at     = toISO8601DateTime . Time.utcToZonedTime tzAsiaTokyo $ ohlcvAt
    , open   = ohlcvOpen
    , high   = ohlcvHigh
    , low    = ohlcvLow
    , close  = ohlcvClose
    , volume = ohlcvVolume
    , source = T.unpack <$> ohlcvSource
    }

--
--
fromApiOhlcv    :: Model.TickerSymbol
                -> Model.TimeFrame
                -> ApiOhlcv
                -> Maybe Model.Ohlcv
fromApiOhlcv ticker timeFrame o = do
    at'<- toUTCTime (at o)
    guard (isValid at')
    Just Model.Ohlcv
        { ohlcvTicker = ticker
        , ohlcvTf     = timeFrame
        , ohlcvAt     = at'
        , ohlcvOpen   = open o
        , ohlcvHigh   = high o
        , ohlcvLow    = low o
        , ohlcvClose  = close o
        , ohlcvVolume = volume o
        , ohlcvSource = T.pack <$> source o
        }
    where
    --
    --
    isValid :: Time.UTCTime -> Bool
    isValid utc =
        let
            t@(Time.TimeOfDay h m s) = Time.localTimeOfDay
                                        $ Time.utcToLocalTime tzAsiaTokyo utc
            open = Time.TimeOfDay 9 0 0
            close = Time.TimeOfDay 15 0 0
        in
        case timeFrame of
            Model.TF1h -> (m == 0) && (s== 0)
            Model.TF1d -> (m == 0) && (s== 0) && (h == 15)
        && (open <= t) && (t <= close)
    --
    --
    toUTCTime :: String -> Maybe Time.UTCTime
    toUTCTime timeStr =
        let parser = Time.parseTimeM True Time.defaultTimeLocale
            format = Time.iso8601DateFormat (Just "%H:%M:%S%z")
        in
        parser format timeStr


