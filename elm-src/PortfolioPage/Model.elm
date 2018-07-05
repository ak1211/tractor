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
   Module      :  PortfolioPage.Model
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Portfolioページのモデル定義です。
-}


module PortfolioPage.Model
    exposing
        ( Model
        , clearModel
        , init
        )

import Generated.WebApi as WebApi
import PortfolioPage.Msg as PortfolioPage
import Task
import Material


type alias Model =
    { accessToken : Maybe String
    , portfolios : Maybe (List WebApi.ApiPortfolio)
    , mdl : Material.Model
    }


clearModel : Model -> Model
clearModel =
    identity


initialModel : Model
initialModel =
    { accessToken = Nothing
    , portfolios = Nothing
    , mdl = Material.model
    }


init : ( Model, Cmd PortfolioPage.Msg )
init =
    initialModel
        ! [ Material.init PortfolioPage.Mdl
          , Task.perform identity <| Task.succeed PortfolioPage.RenewPortfolios
          ]
