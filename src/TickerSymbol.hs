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
Module      :  TickerSymbol
Description :
Copyright   :  (c) 2017 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

-}
{-# LANGUAGE TemplateHaskell #-}

module TickerSymbol where

import           Data.Word           (Word16)
import           Database.Persist.TH

data TickerSymbol   = TSTYO Word16  -- ^ 東証:個別株
                    | TSNI225       -- ^ 日経平均株価
                    | TSTOPIX       -- ^ TOPIX
                    | TSJPXNI400    -- ^ JPX日経インデックス400
    deriving (Show, Read, Eq)

derivePersistField "TickerSymbol"

