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


module Model
    exposing
        ( DocMarkDown
        , Model
        )

import AnalyticsPage.Model as AnalyticsPage
import Generated.WebApi as WebApi
import Material
import PortfolioPage.Model as PortfolioPage
import Route exposing (Route)
import UploadPage.Model as UploadPage


type alias DocMarkDown =
    String


type alias Model =
    { clientID : String
    , accessToken : Maybe WebApi.AccessToken
    , authenticatedUser : Maybe WebApi.AuthenticatedUser
    , pageHistory : List Route
    , serverVersion : Maybe WebApi.VerRev
    , webApiDocument : Maybe DocMarkDown
    , uploadPageModel : UploadPage.Model
    , portfolioPageModel : PortfolioPage.Model
    , analyticsPageModel : AnalyticsPage.Model
    , mdl : Material.Model
    }
