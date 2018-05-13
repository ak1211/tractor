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


module Route exposing (Route(..), routeCaption, toUrlPath, fromLocation)

import Navigation
import UrlParser
import List


type Route
    = Dashboard
    | Upload
    | Portfolio
    | Analytics
    | Reports
    | AccBalance
    | ApiDocument


type alias Item =
    { caption : String
    , path : String
    }


forwardLookup : Route -> Item
forwardLookup rt =
    case rt of
        Dashboard ->
            Item "Dashboard" ""

        Upload ->
            Item "Upload" "upload"

        Portfolio ->
            Item "Portfolio" "portfolio"

        Analytics ->
            Item "Analytics" "analytics"

        Reports ->
            Item "Reports" "reports"

        AccBalance ->
            Item "Account Balance" "acc-balance"

        ApiDocument ->
            Item "API document" "api-document"


routeCaption : Route -> String
routeCaption route =
    (forwardLookup route).caption


toUrlPath : Route -> String
toUrlPath route =
    "/" ++ (forwardLookup route).path


fromLocation : Navigation.Location -> Maybe Route
fromLocation path =
    UrlParser.parsePath parser path


parser : UrlParser.Parser (Route -> a) a
parser =
    let
        f route =
            UrlParser.map route <| UrlParser.s <| (forwardLookup route).path

        paths =
            List.map f
                [ Upload
                , Portfolio
                , Analytics
                , Reports
                , AccBalance
                , ApiDocument
                ]

        top =
            UrlParser.map Dashboard UrlParser.top
    in
        UrlParser.oneOf (top :: paths)
