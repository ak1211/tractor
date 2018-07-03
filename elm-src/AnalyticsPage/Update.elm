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
   Module      :  AnalyticsPage.Update
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Analyticsページのアップデートモジュールです。
-}


module AnalyticsPage.Update exposing (..)

import Generated.WebApi as WebApi
import Http
import Material
import AnalyticsPage.Model as AnalyticsPage
import AnalyticsPage.Msg as AnalyticsPage


update : AnalyticsPage.Msg -> AnalyticsPage.Model -> ( AnalyticsPage.Model, Cmd AnalyticsPage.Msg )
update msg model =
    case msg of
        AnalyticsPage.ChangeAccessToken newToken ->
            { model | accessToken = newToken } ! []

        AnalyticsPage.ChangeMarketCode (Just newMarketCode) ->
            case model.accessToken of
                Just token ->
                    model ! [ getHistories token newMarketCode ]

                Nothing ->
                    { model | quotes = Nothing } ! []

        AnalyticsPage.ChangeMarketCode Nothing ->
            { model | quotes = Nothing } ! []

        AnalyticsPage.UpdateHistories newMarketCode (Ok ohlcvs) ->
            let
                newQuotes =
                    { marketCode = newMarketCode
                    , histories = ohlcvs
                    }
            in
                { model | quotes = Just newQuotes } ! []

        AnalyticsPage.UpdateHistories _ (Err _) ->
            { model | quotes = Nothing } ! []

        AnalyticsPage.SelectTab n ->
            { model | tab = n } ! []

        AnalyticsPage.Mdl subMsg ->
            Material.update AnalyticsPage.Mdl subMsg model


makeAuthorizationHeader : WebApi.AccessToken -> WebApi.AuthzValue
makeAuthorizationHeader token =
    "Bearer " ++ token


getHistories : WebApi.AccessToken -> WebApi.MarketCode -> Cmd AnalyticsPage.Msg
getHistories token marketCode =
    let
        authzHeader =
            makeAuthorizationHeader token
    in
        Http.send (AnalyticsPage.UpdateHistories marketCode) <|
            WebApi.getApiV1StocksHistoryByMarketCode authzHeader marketCode (Just "1d")
