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
   Module      :  Model
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   アプリケーション「Tractor」のフロント側モデル定義です。
-}


module Model exposing (..)

import Material
import Route exposing (Route)
import Generated.WebApi as WebApi
import FileReader


type alias Portfolios =
    List WebApi.Portfolio


type alias Histories =
    List WebApi.Ohlcv


type alias DocMarkDown =
    String


type alias Model =
    { accessToken : Maybe String
    , userName : Maybe String
    , pageHistory : List Route
    , inDropZone : Bool
    , droppedFiles : List FileReader.File
    , serverVersion : Maybe WebApi.VerRev
    , portfolios : Maybe Portfolios
    , histories : Maybe Histories
    , webApiDocument : Maybe DocMarkDown
    , mdl : Material.Model

    -- Boilerplate: model store for any and all Mdl components you use.
    }
