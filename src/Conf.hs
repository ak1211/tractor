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

module Conf
    ( readJSONFile
    , Info (..)
    , InfoSlack (..)
    , InfoMariaDB (..)
    ) where

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Word            (Word16)
import qualified Text.Printf          as Printf

-- | アプリケーションの設定情報
data Info = Info
    { recordAssetsInterval :: Int
    , sendReportInterval   :: Int
    , loginURL             :: String
    , loginID              :: String
    , loginPassword        :: String
    , dealingsPassword     :: String
    , userAgent            :: String
    , slack                :: InfoSlack
    , mariaDB              :: InfoMariaDB
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

-- | パスワードを*に置き換える
hidingPassword :: String -> String
hidingPassword = map (const '*')

-- 型クラスShowのインスタンス
instance Show Info where
    show (Info rai sri url lid lpw dpw ua slk mdb) = unlines
        [ Printf.printf "資産情報取得間隔:%d分" rai
        , Printf.printf "レポート送信間隔:%d分" sri
        , Printf.printf "ログインURL:<%s>" url
        , Printf.printf "ログインID:\"%s\"" $ hidingPassword lid
        , Printf.printf "パスワード:\"%s\"" $ hidingPassword lpw
        , Printf.printf "取引パスワード:\"%s\"" $ hidingPassword dpw
        , Printf.printf "UA:\"%s\"" ua
        , show slk
        , show mdb
        ]

instance Show InfoSlack where
    show (InfoSlack url cha un) = unlines
        [ Printf.printf "Slack Webhook URL:<%s>" $ hidingPassword url
        , Printf.printf "Slack channel:\"%s\"" cha
        , Printf.printf "Slack user name:\"%s\"" un
        ]

instance Show InfoMariaDB where
    show (InfoMariaDB h pt u pw db) = unlines
        [ Printf.printf "MariaDB host:\"%s\"" h
        , Printf.printf "MariaDB port:%d" pt
        , Printf.printf "MariaDB user:\"%s\"" u
        , Printf.printf "MariaDB password:\"%s\"" $ hidingPassword pw
        , Printf.printf "MariaDB database:\"%s\"" db
        ]

$(Aeson.deriveJSON Aeson.defaultOptions ''Info)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoSlack)
$(Aeson.deriveJSON Aeson.defaultOptions ''InfoMariaDB)

-- |
-- 設定ファイル(json)を読み込む
readJSONFile :: String -> IO (Either String Info)
readJSONFile filePath =
    Aeson.eitherDecode <$> BSL.readFile filePath

