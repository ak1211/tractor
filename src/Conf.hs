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
Copyright   :  (c) 2016, 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

アプリケーションの設定情報を管理するモジュールです
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conf
    ( readJSONFile
    , loggingConnInfo
    , connInfoDB
    , Info (..)
    , InfoMatsuiCoJp (..)
    , InfoSlack (..)
    , InfoMariaDB (..)
    ) where

import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.TH          as Aeson
import qualified Data.ByteString.Lazy   as BSL
import           Data.Word              (Word16)
import qualified Database.Persist.MySQL as MySQL

-- | アプリケーションの設定情報
data Info = Info
    { updatePriceMinutes  :: Int
    , noticeAssetsMinutes :: Int
    , userAgent           :: String
    , slack               :: InfoSlack
    , mariaDB             :: InfoMariaDB
    , matsuiCoJp          :: InfoMatsuiCoJp
    } deriving Eq

-- | 通知するSlackの設定情報
data InfoSlack = InfoSlack
    { webHookURL :: String
    , channel    :: String
    , userName   :: String
    } deriving Eq

-- | MariaDBの設定情報
data InfoMariaDB = InfoMariaDB
    { host     :: String
    , port     :: Word16
    , user     :: String
    , password :: String
    , database :: String
    } deriving Eq

-- | 証券会社の設定情報
data InfoMatsuiCoJp = InfoMatsuiCoJp
    { enabled          :: Bool
    , loginURL         :: String
    , loginID          :: String
    , loginPassword    :: String
    , dealingsPassword :: String
    , userAgent        :: String
    } deriving Eq


-- | パスワードを*に置き換える
hiding :: String -> String
hiding = map (const '*')

-- 型クラスShowのインスタンス
instance Show Info where
    show v = unlines
        [ "資産情報取得間隔:"   ++ (show $ updatePriceMinutes v) ++ "分"
        , "レポート送信間隔:"   ++ (show $ noticeAssetsMinutes v) ++ "分"
        , "UA:\""               ++ (userAgent (v::Info)) ++ "\""
        , show (slack v)
        , show (mariaDB v)
        , show (matsuiCoJp v)
        ]

instance Show InfoSlack where
    show v = unlines
        [ "Slack Webhook URL:<" ++ (webHookURL v)
        , "Slack channel:\""    ++ (channel v) ++ "\""
        , "Slack user name:\""  ++ (userName v) ++ "\""
        ]

instance Show InfoMariaDB where
    show v = unlines
        [ "MariaDB host:\""     ++ (host v)  ++ "\""
        , "MariaDB port:"       ++ (show $ port v)
        , "MariaDB user:\""     ++ (user v) ++ "\""
        , "MariaDB password:\"" ++ (hiding $ password v) ++ "\""
        , "MariaDB database:\"" ++ (database v) ++ "\""
        ]

instance Show InfoMatsuiCoJp where
    show v = unlines
        [ "有効:"               ++ (show $ enabled v)
        , "ログインURL:<"       ++ (loginURL v) ++ ">"
        , "ログインID:\""       ++ (hiding $ loginID v) ++ "\""
        , "パスワード:\""       ++ (hiding $ loginPassword v) ++ "\""
        , "取引パスワード:\""   ++ (hiding $ dealingsPassword v) ++ "\""
        , "UA:\""               ++ (userAgent (v::InfoMatsuiCoJp)) ++ "\""
        ]

$(Aeson.deriveJSON Aeson.defaultOptions ''Info)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoSlack)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoMariaDB)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoMatsuiCoJp)

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

