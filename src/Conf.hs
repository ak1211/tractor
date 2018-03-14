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
Module      :  Conf
Description :  read configure information from file
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

アプリケーションの設定情報を管理するモジュールです
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Conf
    ( readJSONFile
    , loggingConnInfo
    , connInfoDB
    , UserAgent
    , Info(..)
    , InfoAccount(..)
    , InfoMatsuiCoJp(..)
    , InfoSBIsecCoJp(..)
    , InfoKabuCom(..)
    , InfoBroker(..)
    , InfoSlack(..)
    , InfoMariaDB(..)
    ) where

import           Data.Aeson                 ((.:), (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.TH              as Aeson
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BSL
import           Data.Monoid                (mempty, (<>))
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import           Data.Word                  (Word16)
import qualified Database.Persist.MySQL     as MySQL
import           GHC.Exts                   (fromList)
import           System.IO                  (Newline (..), nativeNewline)

type UserAgent = String

-- |
-- アプリケーションの設定情報
data Info = Info
    { updatePriceMinutes  :: Int
    , noticeAssetsMinutes :: Int
    , userAgent           :: UserAgent
    , slack               :: InfoSlack
    , mariaDB             :: InfoMariaDB
    , brokers             :: [InfoBroker]
    } deriving Eq

-- |
-- 通知するSlackの設定情報
data InfoSlack = InfoSlack
    { webHookURL :: String
    , token      :: String
    , channel    :: String
    , userName   :: String
    } deriving Eq

-- |
-- MariaDBの設定情報
data InfoMariaDB = InfoMariaDB
    { host     :: String
    , port     :: Word16
    , user     :: String
    , password :: String
    , database :: String
    } deriving Eq

-- |
-- アカウント情報
data InfoAccount = InfoAccount
    { loginID          :: B8.ByteString
    , loginPassword    :: B8.ByteString
    , dealingsPassword :: B8.ByteString
    } deriving Eq

-- |
-- 松井証券
newtype InfoMatsuiCoJp = InfoMatsuiCoJp
    { getInfoMatsuiCoJp :: InfoAccount
    } deriving Eq

-- |
-- SBI証券
newtype InfoSBIsecCoJp = InfoSBIsecCoJp
    { getInfoSBIsecCoJp :: InfoAccount
    } deriving Eq

-- |
-- KabuCom証券
newtype InfoKabuCom = InfoKabuCom
    { getInfoKabuCom :: InfoAccount
    } deriving Eq

-- |
-- 証券会社の設定情報
data InfoBroker
    = MatsuiCoJp InfoMatsuiCoJp
    | SBIsecCoJp InfoSBIsecCoJp
    | KabuCom InfoKabuCom
    deriving Eq

-- |
-- パスワードを*に置き換える
hiding :: String -> String
hiding = map (const '*')

-- |
-- 改行文字
newline :: TLB.Builder
newline =
    TLB.fromString $
    case nativeNewline of
        LF   -> "\n"
        CRLF -> "\r\n"

-- |
-- Infoのshow
instance Show Info where
    show Info{..} =
        TL.unpack . TLB.toLazyText $ mempty
        <> "資産情報取得間隔:" <> TLB.decimal updatePriceMinutes <> "分" <> newline
        <> "レポート送信間隔:" <> TLB.decimal noticeAssetsMinutes <> "分" <> newline
        <> "UA:\"" <> TLB.fromString userAgent <> "\"" <> newline
        <> TLB.fromString (show slack) <> newline
        <> TLB.fromString (show mariaDB) <> newline
        <> TLB.fromString (show brokers) <> newline

-- |
-- InfoSlackのshow
instance Show InfoSlack where
    show InfoSlack{..} =
        TL.unpack . TLB.toLazyText $ mempty
        <> "Slack Webhook URL:<" <> TLB.fromString webHookURL <> newline
        <> "Slack token:\"" <> TLB.fromString token <> "\"" <> newline
        <> "Slack channel:\"" <> TLB.fromString channel <> "\"" <> newline
        <> "Slack user name:\"" <> TLB.fromString userName <> "\"" <> newline

-- |
-- InfoMariaDBのshow
instance Show InfoMariaDB where
    show InfoMariaDB{..} =
        TL.unpack . TLB.toLazyText $ mempty
        <> "MariaDB host:\"" <> TLB.fromString host <> "\"" <> newline
        <> "MariaDB port:" <> TLB.decimal port <> newline
        <> "MariaDB user:\"" <> TLB.fromString user <> "\"" <> newline
        <> "MariaDB password:\"" <> TLB.fromString (hiding password) <> "\"" <> newline
        <> "MariaDB database:\"" <> TLB.fromString database <> "\"" <> newline

-- |
-- InfoAccountのshow
instance Show InfoAccount where
    show InfoAccount{..} =
        TL.unpack . TLB.toLazyText $ mempty
        <> "ログインID:\"" <> (TLB.fromString . hiding $ B8.unpack loginID) <> "\"" <> newline
        <> "パスワード:\"" <> (TLB.fromString . hiding $ B8.unpack loginPassword) <> "\"" <> newline
        <> "取引パスワード:\"" <> (TLB.fromString . hiding $ B8.unpack dealingsPassword) <> "\"" <> newline

-- |
-- InfoMatsuiCoJpのshow
instance Show InfoMatsuiCoJp where
    show (InfoMatsuiCoJp v) = "松井証券: " ++ show v

-- |
-- InfoSBIsecCoJpのshow
instance Show InfoSBIsecCoJp where
    show (InfoSBIsecCoJp v) = "SBI証券: " ++ show v

-- |
-- InfoKabuComのshow
instance Show InfoKabuCom where
    show (InfoKabuCom v) = "Kabu.Com証券: " ++ show v

-- |
-- InfoBrokerのshow
instance Show InfoBroker where
    show = \case
        (MatsuiCoJp v) -> show v
        (SBIsecCoJp v) -> show v
        (KabuCom v) -> show v

-- |
-- InfoAccountのparseJSON
instance Aeson.FromJSON InfoAccount where
    parseJSON = Aeson.withObject "account" $ \o -> do
        loginID         <- B8.pack <$> o .: "loginID"
        loginPassword   <- B8.pack <$> o .: "loginPassword"
        dealingsPassword<- B8.pack <$> o .: "dealingsPassword"
        return InfoAccount{..}

-- |
-- InfoAccountのtoJSON
instance Aeson.ToJSON InfoAccount where
    toJSON InfoAccount{..} = Aeson.object
        [ "loginID"         .= B8.unpack loginID
        , "loginPassword"   .= B8.unpack loginPassword
        , "dealingsPassword".= B8.unpack dealingsPassword
        ]

-- |
-- InfoBrokerのparseJSON
instance Aeson.FromJSON InfoBroker where
    parseJSON = Aeson.withObject "some broker" $ \o -> do
        name <- o .: "name"
        case name of
            "matsui.co.jp" -> MatsuiCoJp . InfoMatsuiCoJp <$> Aeson.parseJSON (Aeson.Object o)
            "sbisec.co.jp" -> SBIsecCoJp . InfoSBIsecCoJp <$> Aeson.parseJSON (Aeson.Object o)
            "kabu.com" -> KabuCom . InfoKabuCom <$> Aeson.parseJSON (Aeson.Object o)
            _              -> fail ("unknown broker: " ++ name)

-- |
-- InfoBrokerのtoJSON
instance Aeson.ToJSON InfoBroker where
    toJSON = \case
        (MatsuiCoJp o) -> Aeson.Object $
            fromList [ ("name", Aeson.String "matsui.co.jp") ] <> toObject (getInfoMatsuiCoJp o)
        (SBIsecCoJp o) -> Aeson.Object $
            fromList [ ("name", Aeson.String "sbisec.co.jp") ] <> toObject (getInfoSBIsecCoJp o)
        (KabuCom o) -> Aeson.Object $
            fromList [ ("name", Aeson.String "kabu.com") ] <> toObject (getInfoKabuCom o)

--
--
toObject:: Aeson.ToJSON a => a -> Aeson.Object
toObject a = case Aeson.toJSON a of
    Aeson.Object o -> o
    _              -> error "toObject: value isn't an Object"

--
--
$(Aeson.deriveJSON Aeson.defaultOptions ''Info)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoSlack)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoMariaDB)

-- |
-- 設定ファイル(json)を読み込む
readJSONFile :: String -> IO (Either String Info)
readJSONFile filePath =
    Aeson.eitherDecode <$> BSL.readFile filePath

-- |
-- MySQLの接続情報
connInfoDB :: InfoMariaDB -> MySQL.ConnectInfo
connInfoDB mdb =
    MySQL.defaultConnectInfo
        { MySQL.connectHost = Conf.host mdb
        , MySQL.connectPort = Conf.port mdb
        , MySQL.connectUser = Conf.user mdb
        , MySQL.connectPassword = Conf.password mdb
        , MySQL.connectDatabase = Conf.database mdb
        }

-- |
-- ログデーターベース情報
loggingConnInfo :: MySQL.ConnectInfo
loggingConnInfo =
    MySQL.defaultConnectInfo
        { MySQL.connectUser = "logginguser"
        , MySQL.connectPassword = "loggingpassword"
        , MySQL.connectDatabase = "stockdb"
        }

