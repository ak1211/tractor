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
Module      :  VerRev
Description :  This software version and revision
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
module VerRev
    ( VerRev
    , versionRevision
    , versionString
    , simpleVersion
    ) where
import qualified Data.Aeson                 as Aeson
import qualified Data.Version               as Version
import qualified Development.GitRev         as GitRev
import           GHC.Generics               (Generic)
import qualified Options.Applicative.Simple
import qualified Paths_tractor              as Meta

--
--
data VerRev = VerRev
    { version        :: String
    , gitBranch      :: String
    , gitHash        :: String
    , gitCommitDate  :: String
    , gitCommitCount :: String
    , gitStatus      :: String
    } deriving (Generic, Show)
instance Aeson.FromJSON VerRev
instance Aeson.ToJSON VerRev

--
--
versionRevision :: VerRev
versionRevision = VerRev
    { version = versionString
    , gitBranch = $(GitRev.gitBranch)
    , gitHash = $(GitRev.gitHash)
    , gitCommitDate = $(GitRev.gitCommitDate)
    , gitCommitCount = $(GitRev.gitCommitCount)
    , gitStatus = if $(GitRev.gitDirty) then "Dirty" else "Clean"
    }

--
--
versionString :: String
versionString =
    Version.showVersion Meta.version

--
--
simpleVersion :: String
simpleVersion =
    $(Options.Applicative.Simple.simpleVersion Meta.version)

