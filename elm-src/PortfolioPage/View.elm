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
   Module      :  PortfolioPage.View
   Description :  This file is web front main module of Application "Tractor"
   Copyright   :  (c) 2016 Akihiro Yamamoto
   License     :  AGPLv3

   Maintainer  :  https://github.com/ak1211
   Stability   :  unstable
   Portability :  POSIX

   Portfolioページのビュー定義です。
-}


module PortfolioPage.View exposing (view)

import Generated.WebApi as WebApi
import Html exposing (Html)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options
import Material.Table as Table
import PortfolioPage.Model as PortfolioPage
import PortfolioPage.Msg as PortfolioPage
import Route exposing (Route)


pinkCard : List (Html m) -> Html m
pinkCard contents =
    Card.view
        [ Options.css "width" "200px"
        , Options.css "margin" "10px"
        , Color.background <| Color.color Color.Pink Color.S500
        ]
        [ Card.text [ Color.text Color.white ] contents
        ]


viewCell : Maybe String -> String
viewCell =
    Maybe.withDefault "Not Available"


view : PortfolioPage.Model -> Html PortfolioPage.Msg
view model =
    let
        viewRow : WebApi.ApiPortfolio -> Html PortfolioPage.Msg
        viewRow item =
            --Table.tr [ Options.onClick <| Msg.GetHistories item.code ]
            Table.tr
                [ Route.Analytics (Just item.code)
                    |> Route.toUrlPath
                    |> PortfolioPage.NewUrl
                    |> Options.onClick
                ]
                [ Table.td [ Table.numeric ] [ Html.text item.code ]
                , Table.td [] [ Html.text <| viewCell item.caption ]
                , Table.td [ Table.numeric ] [ Html.text <| viewCell item.updateAt ]
                ]

        headlines =
            [ Html.text "証券コード"
            , Html.text "銘柄名"
            , Html.text "更新時間"
            ]
    in
        case model.portfolios of
            Just a ->
                viewTable headlines viewRow a

            Nothing ->
                pinkCard [ Html.text "No datasets available" ]


viewTable : List (Html m) -> (a -> Html m) -> List a -> Html m
viewTable headlines viewRow data =
    Table.table []
        [ Table.thead []
            [ Table.tr [] (List.map (\a -> Table.th [] [ a ]) headlines)
            ]
        , Table.tbody [] <| List.map viewRow data
        ]
