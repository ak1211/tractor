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
Module      :  MyHpackConf
Description :  read 'hpack' configulation file
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module ConfHpackageYaml
    ( MyPackageYaml(..)
    ) where
import           Data.Aeson.Types           (typeMismatch)
import           Data.Yaml
import           Language.Haskell.TH.Syntax (Lift)

-- |
--
data MyPackageYaml = MyPackageYaml
    { myName       :: String
    , myVersion    :: String
    , myGitHub     :: String
    , myLicense    :: String
    , myAuthor     :: String
    , myMaintainer :: String
    , myCopyright  :: String
    } deriving (Lift, Eq, Show)

instance FromJSON MyPackageYaml where
    parseJSON (Object v) = MyPackageYaml
        <$> v .: "name"
        <*> v .: "version"
        <*> v .: "github"
        <*> v .: "license"
        <*> v .: "author"
        <*> v .: "maintainer"
        <*> v .: "copyright"
    parseJSON invalid = typeMismatch "MyPackageYaml" invalid

instance ToJSON MyPackageYaml where
    toJSON MyPackageYaml{..} = object
        [ "name"        .= myName
        , "version"     .= myVersion
        , "github"      .= myGitHub
        , "license"     .= myLicense
        , "author"      .= myAuthor
        , "maintainer"  .= myMaintainer
        , "coypright"   .= myCopyright
        ]

