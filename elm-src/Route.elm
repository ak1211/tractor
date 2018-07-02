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
   Module      :  Route
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   アプリケーション「Tractor」のフロント側ルーティング定義です。
-}


module Route
    exposing
        ( QueryError
        , QueryCode
        , QueryState
        , Route(..)
        , showRoute
        , toUrlPath
        , fromLocation
        )

import Navigation
import UrlParser exposing (top, s, (</>), (<?>), stringParam)
import Generated.WebApi as WebApi


type alias QueryError =
    String


type alias QueryCode =
    String


type alias QueryState =
    String


type Route
    = Home (Maybe QueryError) (Maybe QueryCode) (Maybe QueryState)
    | Dashboard
    | Upload
    | Portfolio
    | Analytics (Maybe WebApi.MarketCode)
    | Reports
    | AccBalance
    | ApiDocument


showRoute : Route -> String
showRoute route =
    case route of
        Home _ _ _ ->
            "Home"

        Dashboard ->
            "Dashboard"

        Upload ->
            "Upload"

        Portfolio ->
            "Portfolio"

        Analytics a ->
            "Analytics "
                ++ (Maybe.withDefault "" <| Maybe.map toString a)

        Reports ->
            "Reports"

        AccBalance ->
            "Account Balance"

        ApiDocument ->
            "API Document"


toUrlPath : Route -> String
toUrlPath route =
    case route of
        Home _ _ _ ->
            "/"

        Dashboard ->
            "/"

        Upload ->
            "/#upload"

        Portfolio ->
            "/#portfolio"

        Analytics (Just ts) ->
            "/?ticker=" ++ ts ++ "#analytics"

        Analytics Nothing ->
            "/#analytics"

        Reports ->
            "/#reports"

        AccBalance ->
            "/#acc-balance"

        ApiDocument ->
            "/#api-document"


fromLocation : Navigation.Location -> Maybe Route
fromLocation path =
    let
        parser =
            UrlParser.oneOf
                [ UrlParser.map Home (top <?> stringParam "error" <?> stringParam "code" <?> stringParam "state")
                , UrlParser.map Upload (s "upload")
                , UrlParser.map Portfolio (s "portfolio")
                , UrlParser.map Analytics (s "analytics" <?> stringParam "ticker")
                , UrlParser.map Reports (s "reports")
                , UrlParser.map AccBalance (s "acc-balance")
                , UrlParser.map ApiDocument (s "api-document")
                ]
    in
        case UrlParser.parseHash parser path of
            Just (Home Nothing Nothing Nothing) ->
                Just Dashboard

            a ->
                a
