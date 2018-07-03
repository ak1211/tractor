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
   Module      :  PortfolioPage.Update
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Portfolioページのアップデートモジュールです。
-}


module PortfolioPage.Update exposing (..)

import Generated.WebApi as WebApi
import Http
import Material
import Navigation
import PortfolioPage.Model as PortfolioPage
import PortfolioPage.Msg as PortfolioPage


update : PortfolioPage.Msg -> PortfolioPage.Model -> ( PortfolioPage.Model, Cmd PortfolioPage.Msg )
update msg model =
    case msg of
        PortfolioPage.ChangeAccessToken (Just newToken) ->
            { model | accessToken = Just newToken } ! [ getPortfolios newToken ]

        PortfolioPage.ChangeAccessToken Nothing ->
            { model | accessToken = Nothing } ! []

        PortfolioPage.UpdatePortfolios res ->
            { model | portfolios = Result.toMaybe res } ! []

        PortfolioPage.NewUrl url ->
            model ! [ Navigation.newUrl url ]

        PortfolioPage.Mdl subMsg ->
            Material.update PortfolioPage.Mdl subMsg model


makeAuthorizationHeader : WebApi.AccessToken -> WebApi.AuthzValue
makeAuthorizationHeader token =
    "Bearer " ++ token


getPortfolios : WebApi.AccessToken -> Cmd PortfolioPage.Msg
getPortfolios token =
    let
        authzHeader =
            makeAuthorizationHeader token
    in
        Http.send PortfolioPage.UpdatePortfolios <|
            WebApi.getApiV1Portfolios authzHeader
