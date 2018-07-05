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


module PortfolioPage.Update exposing (update)

import Generated.WebApi as WebApi
import Http
import Material
import Navigation
import Route
import PortfolioPage.Model as PortfolioPage
import PortfolioPage.Msg as PortfolioPage


update : PortfolioPage.Msg -> PortfolioPage.Model -> ( PortfolioPage.Model, Cmd PortfolioPage.Msg )
update msg model =
    case msg of
        PortfolioPage.ChangeAccessToken (Just newToken) ->
            { model | accessToken = Just newToken } ! [ getPortfolios ]

        PortfolioPage.ChangeAccessToken Nothing ->
            { model | accessToken = Nothing } ! []

        PortfolioPage.RenewPortfolios ->
            model ! [ getPortfolios ]

        PortfolioPage.DoneGetPortfolios res ->
            { model | portfolios = Result.toMaybe res } ! []

        PortfolioPage.UrlChange Route.Portfolio ->
            -- 自ページ内に移動
            model ! []

        PortfolioPage.UrlChange _ ->
            -- 他のページに移動
            PortfolioPage.clearModel model ! []

        PortfolioPage.NewUrl url ->
            model ! [ Navigation.newUrl url ]

        PortfolioPage.Mdl subMsg ->
            Material.update PortfolioPage.Mdl subMsg model


getPortfolios : Cmd PortfolioPage.Msg
getPortfolios =
    Http.send PortfolioPage.DoneGetPortfolios WebApi.getApiV1Portfolios
