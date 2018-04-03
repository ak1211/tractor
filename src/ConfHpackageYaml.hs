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
Module      :  ConfHpackageYaml
Description :  read 'hpack' configulation file
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift    #-}
{-# LANGUAGE StrictData    #-}
module ConfHpackageYaml
    ( MyPackageYaml(..)
    ) where
import           Data.Aeson
import           Data.Char                  (toLower)
import           GHC.Generics
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
    } deriving (Generic, Lift, Eq, Show)
instance FromJSON MyPackageYaml where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = map toLower . drop 2 }
instance ToJSON MyPackageYaml where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = map toLower . drop 2 }

