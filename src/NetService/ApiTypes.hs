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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NetService.ApiTypes
    ( SVG
    , AuthTempCode(..)
    , SvgBinary(..)
    , ServerTChan
    , QueryLimit(unQueryLimit)
    , makeQueryLimit
    , SystemSignal(..)
    , SystemHealth(..)
    , AuthClientId(..)
    , ApiPortfolio
    , toApiPortfolio
    , ApiOhlcv
    , toApiOhlcv
    , fromApiOhlcv
    , UsersInfoResponse(..)
    , OAuthAccessResponse(..)
    , AuthenticatedUser(..)
    , RespAuth(..)
    , VerRev(..)
    )
where
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                             ( (&)
                                                          , (?~)
                                                          )
import qualified Crypto.JWT                    as Jose
import           Data.Aeson                               ( (.:)
                                                          , (.:?)
                                                          )
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Csv                                 ( (.=) )
import qualified Data.Csv                      as Csv
import           Data.Default                             ( Default(..) )
import           Data.Int                                 ( Int64 )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import           GHC.Generics                             ( Generic )
import           Network.HTTP.Media                       ( (//) )
import qualified Servant
import           Servant.API                              ( Accept
                                                          , MimeRender
                                                          , contentType
                                                          , mimeRender
                                                          )
import qualified Servant.Auth.Server           as Auth
import qualified Servant.Docs
import           Servant.Elm                              ( ElmType )
import qualified Web.FormUrlEncoded

import           Lib                                      ( toISO8601DateTime
                                                          , tzAsiaTokyo
                                                          )
import qualified Model


-- |
--
newtype SvgBinary = SvgBinary { unSvgBinary :: BL8.ByteString }

data SVG

instance Accept SVG where
    contentType _ = "image" // "svg+xml"

instance MimeRender SVG SvgBinary where
    mimeRender _ = unSvgBinary

-- |
--
newtype AuthTempCode = AuthTempCode
    { code :: String
    } deriving (Show, Generic)

instance Aeson.FromJSON AuthTempCode
instance Aeson.ToJSON AuthTempCode

instance ElmType AuthTempCode
instance Servant.Docs.ToSample AuthTempCode where
    toSamples _ =
        Servant.Docs.singleSample (AuthTempCode "code in OAuth flow")

-- |
--
newtype AuthClientId = AuthClientId
    { clientid :: T.Text
    } deriving (Show, Generic)

instance Aeson.FromJSON AuthClientId
instance Aeson.ToJSON AuthClientId

instance ElmType AuthClientId
instance Servant.Docs.ToSample AuthClientId where
    toSamples _ =
        Servant.Docs.singleSample (AuthClientId "oauth client identifier")

-- |
--
newtype QueryLimit = MkQueryLimit
    { unQueryLimit :: Int
    } deriving Generic

instance ElmType QueryLimit

instance Default QueryLimit where
    def = MkQueryLimit 1000

instance Servant.FromHttpApiData QueryLimit where
    parseQueryParam x =
        case makeQueryLimit =<< parse x of
            Nothing -> Left "QueryLimit"
            Just a  -> Right a
        where
        parse :: T.Text -> Maybe Int
        parse =
            either (const Nothing) Just . Servant.parseQueryParam

-- |
-- スマートコンストラクタ
makeQueryLimit :: Int -> Maybe QueryLimit
makeQueryLimit x | x > 0     = Just (MkQueryLimit x)
                 | otherwise = Nothing

-- |
--
data SystemSignal
    = Green
    | Yellow
    | Red
    deriving Generic

instance Aeson.FromJSON SystemSignal
instance Aeson.ToJSON SystemSignal

instance ElmType SystemSignal
instance Servant.Docs.ToSample SystemSignal where
    toSamples _ =
        Servant.Docs.noSamples

data SystemHealth = SystemHealth
    { system :: SystemSignal
    } deriving Generic

instance Aeson.FromJSON SystemHealth
instance Aeson.ToJSON SystemHealth

instance ElmType SystemHealth
instance Servant.Docs.ToSample SystemHealth where
    toSamples _ =
        Servant.Docs.samples
        [ SystemHealth Green
        , SystemHealth Yellow
        , SystemHealth Red
        ]

-- |
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

-- |
--
type ServerTChan = STM.TChan [ApiOhlcv]

-- |
-- slack api users.infoからのJSON返答
data UsersInfoResponse = UsersInfoResponse
    { respOk                :: Bool
    , respError             :: Maybe T.Text
    , respUserId            :: Maybe T.Text
    , respTeamId            :: Maybe T.Text
    , respUserName          :: Maybe T.Text
    , respUserDeleted       :: Maybe Bool
    , respUserColor         :: Maybe T.Text
    , respUserRealName      :: Maybe T.Text
    , respUserTz            :: Maybe T.Text
    , respUserTzLabel       :: Maybe T.Text
    , respUserTzOffset      :: Maybe Int
    --
    -- profileは省略する
    , respIsAdmin           :: Maybe Bool
    , respIsOwner           :: Maybe Bool
    , respIsPrimaryOwner    :: Maybe Bool
    , respIsRestricted      :: Maybe Bool
    , respIsUltraRestricted :: Maybe Bool
    , respIsBot             :: Maybe Bool
    , respUpdated           :: Maybe Bool
    , respIsAppUser         :: Maybe Bool
    , respHas2FA            :: Maybe Bool
    } deriving Show

instance Aeson.FromJSON UsersInfoResponse where
    parseJSON = Aeson.withObject "users.info response" $ \o -> do
        respOk                <- o .: "ok"
        respError             <- o .:? "error"
        --
        -- userの中身を取り出す
        oUser                 <- o .:? "user"
        respUserId            <- maybe (pure Nothing) (.:? "id") oUser
        respTeamId            <- maybe (pure Nothing) (.:? "team_id") oUser
        respUserName          <- maybe (pure Nothing) (.:? "name") oUser
        respUserDeleted       <- maybe (pure Nothing) (.:? "deleted") oUser
        respUserColor         <- maybe (pure Nothing) (.:? "color") oUser
        respUserRealName      <- maybe (pure Nothing) (.:? "real_name") oUser
        respUserTz            <- maybe (pure Nothing) (.:? "tz") oUser
        respUserTzLabel       <- maybe (pure Nothing) (.:? "tz_label") oUser
        respUserTzOffset      <- maybe (pure Nothing) (.:? "tz_offset") oUser
        --
        --
        respIsAdmin           <- o .:? "is_admin"
        respIsOwner           <- o .:? "is_owner"
        respIsPrimaryOwner    <- o .:? "is_primary_owner"
        respIsRestricted      <- o .:? "is_restricted"
        respIsUltraRestricted <- o .:? "is_ultra_restricted"
        respIsBot             <- o .:? "is_bot"
        respUpdated           <- o .:? "updated"
        respIsAppUser         <- o .:? "is_app_user"
        respHas2FA            <- o .:? "has_2fa"
        return UsersInfoResponse{..}

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
-- AuthenticatedUser
data AuthenticatedUser = AuthenticatedUser
    { userId       :: T.Text
    , userName     :: T.Text
    , userRealName :: T.Text
    , userTz       :: T.Text
    , userTzOffset :: Int
    } deriving (Eq, Show, Generic)

instance Aeson.FromJSON AuthenticatedUser
instance Aeson.ToJSON AuthenticatedUser

instance Auth.FromJWT AuthenticatedUser
instance Auth.ToJWT AuthenticatedUser where
    encodeJWT a =
        Jose.addClaim "dat" (Aeson.toJSON a) claims
        where
        --
        -- ToDo:
        claims = Jose.emptyClaimsSet
                    & Jose.claimIss ?~ "issuer"

instance Servant.Elm.ElmType AuthenticatedUser
instance Servant.Docs.ToSample AuthenticatedUser where
    toSamples _ =
        Servant.Docs.singleSample v
        where
            v = AuthenticatedUser
                "xxxx"
                "John Doe"
                "Real John Doe"
                "America/Los_Angeles"
                (-25200)

-- |
--
data RespAuth = RespAuth
    { accessToken :: T.Text
    , expiresIn   :: Int
    , tokenType   :: T.Text
    } deriving (Eq, Show, Generic)
instance Aeson.FromJSON RespAuth
instance Aeson.ToJSON RespAuth
instance Auth.FromJWT RespAuth
instance Auth.ToJWT RespAuth
instance Servant.Elm.ElmType RespAuth
instance Servant.Docs.ToSample RespAuth where
    toSamples _ =
        Servant.Docs.singleSample $
        RespAuth "API Access Token" 3920 "Bearer"

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
        .   Time.utcToZonedTime tzAsiaTokyo
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

-- |
-- >>> :{
-- >>> toApiOhlcv $ Model.Ohlcv { Model.ohlcvTicker = Model.TSTYO 1320
-- >>>                          , Model.ohlcvTf     = Model.TF1d
-- >>>                          , Model.ohlcvAt     = Time.UTCTime (Time.ModifiedJulianDay 0) 0
-- >>>                          , Model.ohlcvOpen   = Just 23250.0
-- >>>                          , Model.ohlcvHigh   = Just 23340.0
-- >>>                          , Model.ohlcvLow    = Just 23230.0
-- >>>                          , Model.ohlcvClose  = Just 23330.0
-- >>>                          , Model.ohlcvVolume = 31384
-- >>>                          , Model.ohlcvSource = Nothing
-- >>>                          }
-- >>> :}
-- ApiOhlcv {at = "1858-11-17T09:00:00+0900", open = Just 23250.0, high = Just 23340.0, low = Just 23230.0, close = Just 23330.0, volume = 31384, source = Nothing}
--
toApiOhlcv :: Model.Ohlcv -> ApiOhlcv
toApiOhlcv Model.Ohlcv {..} = ApiOhlcv
    { at     = toISO8601DateTime . Time.utcToZonedTime tzAsiaTokyo $ ohlcvAt
    , open   = ohlcvOpen
    , high   = ohlcvHigh
    , low    = ohlcvLow
    , close  = ohlcvClose
    , volume = ohlcvVolume
    , source = T.unpack <$> ohlcvSource
    }

-- |
-- >>> :{
-- >>> fromApiOhlcv (Model.TSTYO 1320) Model.TF1d
-- >>>              (ApiOhlcv { at     = "2018-06-22T00:00:00+0900"
-- >>>                        , open   = Just 23250.0
-- >>>                        , high   = Just 23340.0
-- >>>                        , low    = Just 23230.0
-- >>>                        , close  = Just 23330.0
-- >>>                        , volume = 31384
-- >>>                        , source = Nothing
-- >>>                        })
-- >>> :}
-- Left "out of trading hours"
--
-- >>> :{
-- >>> fromApiOhlcv (Model.TSTYO 1320) Model.TF1d
-- >>>              (ApiOhlcv { at     = "2018-06-22T15:00:00+0900"
-- >>>                        , open   = Just 23250.0
-- >>>                        , high   = Just 23340.0
-- >>>                        , low    = Just 23230.0
-- >>>                        , close  = Just 23330.0
-- >>>                        , volume = 31384
-- >>>                        , source = Nothing
-- >>>                        })
-- >>> :}
-- Right (Ohlcv {ohlcvTicker = TSTYO 1320, ohlcvTf = TF1d, ohlcvAt = 2018-06-22 06:00:00 UTC, ohlcvOpen = Just 23250.0, ohlcvHigh = Just 23340.0, ohlcvLow = Just 23230.0, ohlcvClose = Just 23330.0, ohlcvVolume = 31384, ohlcvSource = Nothing})
--
fromApiOhlcv
    :: Model.TickerSymbol
    -> Model.TimeFrame
    -> ApiOhlcv
    -> Either String Model.Ohlcv
fromApiOhlcv ticker timeFrame o = do
    time <- validation =<< toUTCTime (at o)
    Right Model.Ohlcv
        { ohlcvTicker = ticker
        , ohlcvTf     = timeFrame
        , ohlcvAt     = time
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
    tokyoTime = Time.localTimeOfDay . Time.utcToLocalTime tzAsiaTokyo
    --
    --
    validation :: Time.UTCTime -> Either String Time.UTCTime
    validation utc = inTradingHours utc >> inTimeFrame utc
    --
    --
    inTradingHours utc =
        let t = tokyoTime utc
        in  if Time.TimeOfDay 9 0 0 <= t && t <= Time.TimeOfDay 15 0 0
                then Right utc
                else Left "out of trading hours"
    --
    --
    inTimeFrame utc =
        let t@(Time.TimeOfDay _ m s) = tokyoTime utc
        in  case timeFrame of
                Model.TF1h | m == 0 && s == 0 -> Right utc
                Model.TF1d | Time.TimeOfDay 15 0 0 == t -> Right utc
                _                             -> Left "out of time frame"
    --
    --
    toUTCTime timeStr =
        let parser = Time.parseTimeM True Time.defaultTimeLocale
            format = Time.iso8601DateFormat (Just "%H:%M:%S%z")
        in  parser format timeStr


