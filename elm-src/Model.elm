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
import UploadPage.Model as UploadPage


type alias Portfolios =
    List WebApi.ApiPortfolio


type alias UploadFileContent =
    { marketCode : WebApi.MarketCode
    , timeFrame : WebApi.TimeFrame
    , ohlcvs : List WebApi.ApiOhlcv
    }


type alias DocMarkDown =
    String


type alias Model =
    { clientID : String
    , accessToken : Maybe String
    , userName : Maybe String
    , pageHistory : List Route
    , uploadFilesContent : List UploadFileContent
    , serverVersion : Maybe WebApi.VerRev
    , portfolios : Maybe Portfolios
    , histories : Maybe (List WebApi.ApiOhlcv)
    , webApiDocument : Maybe DocMarkDown
    , uploadPageModel : UploadPage.Model
    , mdl : Material.Model
    }


initialModel : String -> Model
initialModel initClientID =
    { clientID = initClientID
    , accessToken = Nothing
    , userName = Nothing
    , pageHistory = []
    , uploadFilesContent = []
    , serverVersion = Nothing
    , portfolios = Nothing
    , histories = Nothing
    , webApiDocument = Nothing
    , uploadPageModel = UploadPage.initialModel
    , mdl = Material.model
    }
