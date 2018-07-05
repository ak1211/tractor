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
   Module      :  AnalyticsPage.Model
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Analyticsページのモデル定義です。
-}


module AnalyticsPage.Model exposing (Model, clearModel, init)

import Generated.WebApi as WebApi
import AnalyticsPage.Msg as AnalyticsPage
import Material


type alias Model =
    { accessToken : Maybe WebApi.AccessToken
    , marketCode : Maybe WebApi.MarketCode
    , histories : List WebApi.ApiOhlcv
    , tab : Int
    , mdl : Material.Model
    }


clearModel : Model -> Model
clearModel model =
    { model
        | marketCode = Nothing
        , histories = []
        , tab = 0
    }


initialModel : Model
initialModel =
    { accessToken = Nothing
    , marketCode = Nothing
    , histories = []
    , tab = 0
    , mdl = Material.model
    }


init : ( Model, Cmd AnalyticsPage.Msg )
init =
    initialModel ! [ Material.init AnalyticsPage.Mdl ]
